module LLVM.Codegen.IRBuilder
  ( IRBuilderT
  , IRBuilder
  , block
  , named
  , emitInstr
  , emitInstrVoid
  , emitTerminator
  , BasicBlock(..)
  , runIRBuilderT
  , runIRBuilder
  , MonadIRBuilder

  , add
  , mul
  , sub
  , udiv
  , and
  , trunc
  , zext
  , ptrtoint
  , bitcast
  , icmp
  , alloca
  , gep
  , load
  , store
  , phi
  , call
  , ret
  , retVoid
  , br
  , condBr
  , switch
  , select

  , bit
  , int8
  , int16
  , int32
  , int64
  , intN
  ) where

import Prelude hiding (and)
import GHC.Stack
import Control.Monad.State
import Control.Monad.Reader
import Data.Functor.Identity
import qualified Data.List.NonEmpty as NE
import qualified Data.DList as DList
import Data.DList (DList)
import Data.Monoid
import Data.Maybe
import Data.Word
import LLVM.NameSupply
import LLVM.Codegen.Operand
import LLVM.Codegen.Type
import LLVM.Codegen.IR
import LLVM.Pretty hiding (align)


data BasicBlock
  = BB
  { bbName :: Name
  , bbInstructions :: DList (Maybe Operand, IR)
  , bbTerminator :: Terminator
  } deriving Show

data PartialBlock
  = PartialBlock
  { pbName :: Name
  , pbInstructions :: DList (Maybe Operand, IR)
  , pbTerminator :: First Terminator
  }

data IRBuilderState
  = IRBuilderState
  { basicBlocks :: DList BasicBlock
  , currentBlock :: PartialBlock
  }

newtype IRBuilderT m a
  = IRBuilderT (StateT IRBuilderState (NameSupplyT m) a)
  deriving ( Functor, Applicative, Monad, MonadFix, MonadIO
           , MonadReader (Maybe Name), MonadState IRBuilderState
           , MonadNameSupply
           )
  via StateT IRBuilderState (NameSupplyT m)

type IRBuilder = IRBuilderT Identity

runIRBuilderT :: Monad m => IRBuilderT m a -> m [BasicBlock]
runIRBuilderT (IRBuilderT m) = do
  let partialBlock = PartialBlock (Name "start") mempty mempty
      result = runNameSupplyT $ execStateT m (IRBuilderState mempty partialBlock)
  previousBlocks <- fmap (flip DList.apply mempty . basicBlocks) result
  currentBlk <- fmap currentBlock result
  pure $ previousBlocks ++ [partialBlockToBasicBlock currentBlk]

runIRBuilder :: IRBuilder a -> [BasicBlock]
runIRBuilder = runIdentity . runIRBuilderT

partialBlockToBasicBlock :: PartialBlock -> BasicBlock
partialBlockToBasicBlock pb =
  let currentTerm = fromMaybe (Terminator $ Ret Nothing) $ getFirst $ pbTerminator pb
  in BB (pbName pb) (pbInstructions pb) currentTerm

type MonadIRBuilder m =
  ( MonadState IRBuilderState m
  , MonadReader (Maybe Name) m
  , MonadFix m
  , MonadNameSupply m
  )

block :: MonadIRBuilder m => m Name
block = do
  blockName <- ask >>= \case
    Nothing -> do
      fresh `named` "block"
    Just _sugg ->
      fresh

  modify $ \s ->
    let currBlock = currentBlock s
        hasntStartedBlock = null (DList.toList (pbInstructions currBlock)) && isNothing (getFirst (pbTerminator currBlock))
        blocks = basicBlocks s
     in s { basicBlocks =
              if hasntStartedBlock
                then blocks
                else DList.snoc blocks (partialBlockToBasicBlock currBlock)
          , currentBlock = PartialBlock blockName mempty mempty
          }
  pure blockName

-- NOTE: Only used internally, this creates an unassigned operand
mkOperand :: MonadIRBuilder m => Type -> m Operand
mkOperand ty = do
  name <- fresh
  pure $ LocalRef ty name

emitInstr :: MonadIRBuilder m => Type -> IR -> m Operand
emitInstr ty instr = do
  operand <- mkOperand ty
  addInstrToCurrentBlock (Just operand) instr
  pure operand

emitInstrVoid :: MonadIRBuilder m => IR -> m ()
emitInstrVoid instr =
  addInstrToCurrentBlock Nothing instr

addInstrToCurrentBlock :: MonadIRBuilder m => Maybe Operand -> IR -> m ()
addInstrToCurrentBlock operand instr =
  modifyCurrentBlock $ \blk ->
    let instrs = DList.snoc (pbInstructions blk) (operand, instr)
      in blk { pbInstructions = instrs }

emitTerminator :: MonadIRBuilder m => Terminator -> m ()
emitTerminator term =
  modifyCurrentBlock $ \blk ->
    blk { pbTerminator = First (Just term) <> pbTerminator blk }

modifyCurrentBlock :: MonadIRBuilder m => (PartialBlock -> PartialBlock) -> m ()
modifyCurrentBlock f =
  modify $ \s -> s { currentBlock = f (currentBlock s) }


-- Helpers for generating instructions:

add :: MonadIRBuilder m => Operand -> Operand -> m Operand
add lhs rhs =
  emitInstr (typeOf lhs) $ Add False False lhs rhs

mul :: MonadIRBuilder m => Operand -> Operand -> m Operand
mul lhs rhs =
  emitInstr (typeOf lhs) $ Mul False False lhs rhs

sub :: MonadIRBuilder m => Operand -> Operand -> m Operand
sub lhs rhs =
  emitInstr (typeOf lhs) $ Sub False False lhs rhs

udiv :: MonadIRBuilder m => Operand -> Operand -> m Operand
udiv lhs rhs =
  emitInstr (typeOf lhs) $ Udiv False lhs rhs

and :: MonadIRBuilder m => Operand -> Operand -> m Operand
and lhs rhs =
  emitInstr (typeOf lhs) $ And lhs rhs

trunc :: MonadIRBuilder m => Operand -> Type -> m Operand
trunc val ty =
  emitInstr ty $ Trunc val ty

zext :: MonadIRBuilder m => Operand -> Type -> m Operand
zext val ty =
  emitInstr ty $ Zext val ty

ptrtoint :: MonadIRBuilder m => Operand -> Type -> m Operand
ptrtoint val ty =
  emitInstr ty $ PtrToInt val ty

bitcast :: MonadIRBuilder m => Operand -> Type -> m Operand
bitcast val ty =
  emitInstr ty $ Bitcast val ty

icmp :: MonadIRBuilder m => ComparisonType -> Operand -> Operand -> m Operand
icmp cmp a b =
  emitInstr i1 $ ICmp cmp a b

alloca :: MonadIRBuilder m => Type -> (Maybe Operand) -> Int -> m Operand
alloca ty numElems alignment =
  emitInstr ty $ Alloca ty numElems alignment

gep :: (HasCallStack, MonadIRBuilder m) => Operand -> [Operand] -> m Operand
gep operand indices = do
  let resultType = computeGepType (typeOf operand) indices
  case resultType of
    Left err -> error err -- TODO
    Right ty ->
      emitInstr ty $ GetElementPtr False operand indices

load :: MonadIRBuilder m => Operand -> Alignment -> m Operand
load addr align =
  case typeOf addr of
    PointerType ty ->
      emitInstr ty $ Load False addr Nothing align
    _ ->
      error "Malformed AST: Expected a pointer type"

store :: MonadIRBuilder m => Operand -> Alignment -> Operand -> m ()
store addr align value =
  emitInstrVoid $ Store False addr value Nothing align

computeGepType :: HasCallStack => Type -> [Operand] -> Either String Type
computeGepType ty [] = Right $ PointerType ty
computeGepType (PointerType ty) (_:is) = computeGepType ty is
computeGepType ty _ =
  Left $ "Expecting aggregate type. (Malformed AST): " <> show ty

  {-
TODO: add support for structs, arrays, ...
indexTypeByOperands :: (HasCallStack, MonadModuleBuilder m) => Type -> [Operand] -> m (Either String Type)
indexTypeByOperands (StructureType _ elTys) (ConstantOperand (C.Int 32 val):is) =
  indexTypeByOperands (elTys !! fromIntegral val) is
indexTypeByOperands (StructureType _ _) (i:_) =
  return $ Left $ "Indices into structures should be 32-bit integer constants. (Malformed AST): " ++ show i
indexTypeByOperands (VectorType _ elTy) (_:is) = indexTypeByOperands elTy is
indexTypeByOperands (ArrayType _ elTy) (_:is) = indexTypeByOperands elTy is
indexTypeByOperands (NamedTypeReference n) is = do
  mayTy <- liftModuleState (gets (Map.lookup n . builderTypeDefs))
  case mayTy of
    Nothing -> return $ Left $ "Couldn’t resolve typedef for: " ++ show n
    Just ty -> indexTypeByOperands ty is
    -}

phi :: (HasCallStack, MonadIRBuilder m) => [(Operand, Name)] -> m Operand
phi cases
  | null cases = error "phi instruction should always have > 0 cases!"
  | otherwise =
    let neCases = NE.fromList cases
        ty = typeOf $ fst $ NE.head neCases
     in emitInstr ty $ Phi neCases

call :: (HasCallStack, MonadIRBuilder m) => Operand -> [Operand] -> m Operand
call fn args = case typeOf fn of
  FunctionType retTy _->
    emitCallInstr retTy
  PointerType (FunctionType retTy _) ->
    emitCallInstr retTy
  _ -> error "Malformed AST, expected function type in 'call' instruction"
  where
    emitCallInstr resultTy =
      emitInstr resultTy $ Call Nothing C fn args

ret :: MonadIRBuilder m => Operand -> m ()
ret val =
  emitTerminator (Terminator (Ret (Just val)))

retVoid :: MonadIRBuilder m => m ()
retVoid =
  emitTerminator (Terminator (Ret Nothing))

br :: MonadIRBuilder m => Name -> m ()
br label =
  emitTerminator (Terminator (Br label))

condBr :: MonadIRBuilder m => Operand -> Name -> Name -> m ()
condBr cond trueLabel falseLabel =
  emitTerminator (Terminator (CondBr cond trueLabel falseLabel))

switch :: MonadIRBuilder m => Operand -> Name -> [(Operand, Name)] -> m ()
switch value defaultDest dests =
  emitTerminator $ Terminator $ Switch value defaultDest dests

select :: MonadIRBuilder m => Operand -> Operand -> Operand -> m Operand
select c t f =
  emitInstr (typeOf t) $ Select c t f

instance Pretty BasicBlock where
  pretty (BB (Name name) stmts (Terminator term)) =
    let prettyStmts = indent 2 $ vsep $ (map (uncurry prettyStmt) $ DList.apply stmts []) ++ [pretty term]
     in vsep [ pretty name <> ":", prettyStmts ]
    where
      prettyStmt operand instr =
        let instrDoc = pretty instr
         in maybe instrDoc (\op -> pretty op <+> "=" <+> instrDoc) operand

bit :: Bool -> Operand
bit b =
  intN 1 $ if b then 1 else 0

int8 :: Int -> Operand
int8 =
  intN 8 . toInteger

int16 :: Int -> Operand
int16 =
  intN 16 . toInteger

int32 :: Int -> Operand
int32 =
  intN 32 . toInteger

int64 :: Int -> Operand
int64 =
  intN 64 . toInteger

intN :: Word32 -> Integer -> Operand
intN bits value =
  ConstantOperand $ Int bits value
