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
  , ret
  , retVoid
  , br
  , condBr
  , switch
  , select
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
  deriving ( Functor, Applicative, Monad, MonadFix
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

block :: Monad m => IRBuilderT m Name
block = do
  blockName <- ask >>= \case
    Nothing -> do
      name <- fresh
      pure $ Name $ "block_" <> unName name
    Just _sugg ->
      fresh

  modify $ \s ->
    let currBlock = currentBlock s
        blocks = basicBlocks s
     in s { basicBlocks = DList.snoc blocks (partialBlockToBasicBlock currBlock)
          , currentBlock = PartialBlock blockName mempty mempty
          }
  pure blockName

partialBlockToBasicBlock :: PartialBlock -> BasicBlock
partialBlockToBasicBlock pb =
  let currentTerm = fromMaybe (Terminator $ Ret Nothing) $ getFirst $ pbTerminator pb
  in BB (pbName pb) (pbInstructions pb) currentTerm

-- NOTE: Only used internally, this creates an unassigned operand
mkOperand :: Monad m => Type -> IRBuilderT m Operand
mkOperand ty = do
  name <- fresh
  pure $ LocalRef ty name

emitInstr :: Monad m => Type -> IR -> IRBuilderT m Operand
emitInstr ty instr = do
  operand <- mkOperand ty
  addInstrToCurrentBlock (Just operand) instr
  pure operand

emitInstrVoid :: Monad m => IR -> IRBuilderT m ()
emitInstrVoid instr =
  addInstrToCurrentBlock Nothing instr

addInstrToCurrentBlock :: Monad m => Maybe Operand -> IR -> IRBuilderT m ()
addInstrToCurrentBlock operand instr =
  modifyCurrentBlock $ \blk ->
    let instrs = DList.snoc (pbInstructions blk) (operand, instr)
      in blk { pbInstructions = instrs }

emitTerminator :: Monad m => Terminator -> IRBuilderT m ()
emitTerminator term =
  modifyCurrentBlock $ \blk ->
    blk { pbTerminator = First (Just term) <> pbTerminator blk }

modifyCurrentBlock :: Monad m => (PartialBlock -> PartialBlock) -> IRBuilderT m ()
modifyCurrentBlock f =
  modify $ \s -> s { currentBlock = f (currentBlock s) }


-- Helpers for generating instructions:

add :: Monad m => Operand -> Operand -> IRBuilderT m Operand
add lhs rhs =
  emitInstr (typeOf lhs) $ Add False False lhs rhs

mul :: Monad m => Operand -> Operand -> IRBuilderT m Operand
mul lhs rhs =
  emitInstr (typeOf lhs) $ Mul False False lhs rhs

sub :: Monad m => Operand -> Operand -> IRBuilderT m Operand
sub lhs rhs =
  emitInstr (typeOf lhs) $ Sub False False lhs rhs

udiv :: Monad m => Operand -> Operand -> IRBuilderT m Operand
udiv lhs rhs =
  emitInstr (typeOf lhs) $ Udiv False lhs rhs

and :: Monad m => Operand -> Operand -> IRBuilderT m Operand
and lhs rhs =
  emitInstr (typeOf lhs) $ And lhs rhs

trunc :: Monad m => Operand -> Type -> IRBuilderT m Operand
trunc val ty =
  emitInstr (typeOf val) $ Trunc val ty

zext :: Monad m => Operand -> Type -> IRBuilderT m Operand
zext val ty =
  emitInstr (typeOf val) $ Zext val ty

ptrtoint :: Monad m => Operand -> Type -> IRBuilderT m Operand
ptrtoint val ty =
  emitInstr (typeOf val) $ PtrToInt val ty

bitcast :: Monad m => Operand -> Type -> IRBuilderT m Operand
bitcast val ty =
  emitInstr (typeOf val) $ Bitcast val ty

icmp :: Monad m => ComparisonType -> Operand -> Operand -> IRBuilderT m Operand
icmp cmp a b =
  emitInstr (typeOf a) $ ICmp cmp a b

alloca :: Monad m => Type -> (Maybe Operand) -> Int -> IRBuilderT m Operand
alloca ty numElems alignment =
  emitInstr ty $ Alloca ty numElems alignment

gep :: (HasCallStack, Monad m) => Operand -> [Operand] -> IRBuilderT m Operand
gep operand indices = do
  let resultType = computeGepType (typeOf operand) indices
  case resultType of
    Left err -> error err -- TODO
    Right ty ->
      emitInstr ty $ GetElementPtr False operand indices

load :: Monad m => Operand -> Word32 -> IRBuilderT m Operand
load addr align =
  undefined -- TODO

store :: Monad m => Operand -> Word32 -> Operand -> IRBuilderT m ()
store addr align value =
  emitInstrVoid $ Store False addr value Nothing align

phi :: Monad m => [(Operand, Name)] -> IRBuilderT m Operand
phi cases
  | null cases = error "phi instruction should always have > 0 cases!"
  | otherwise =
    let neCases = NE.fromList cases
        ty = typeOf $ fst $ NE.head neCases
     in emitInstr ty $ Phi neCases

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

ret :: Monad m => Operand -> IRBuilderT m ()
ret val =
  emitTerminator (Terminator (Ret (Just val)))

retVoid :: Monad m => IRBuilderT m ()
retVoid =
  emitTerminator (Terminator (Ret Nothing))

br :: Monad m => Name -> IRBuilderT m ()
br label =
  emitTerminator (Terminator (Br label))

condBr :: Monad m => Operand -> Name -> Name -> IRBuilderT m ()
condBr cond trueLabel falseLabel =
  emitTerminator (Terminator (CondBr cond trueLabel falseLabel))

switch :: Monad m => Operand -> Name -> [(Operand, Name)] -> IRBuilderT m ()
switch value defaultDest dests =
  emitTerminator $ Terminator $ Switch value defaultDest dests

select :: Monad m => Operand -> Operand -> Operand -> IRBuilderT m Operand
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

