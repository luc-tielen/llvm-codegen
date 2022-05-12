module LLVM.Codegen.IRBuilder
  ( IRBuilderT
  , IRBuilder
  , block
  , named
  , emitInstr
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
  , ret
  , retVoid
  , br
  , condBr
  -- , switch  TODO
  ) where

import Prelude hiding (and)
import Control.Monad.State
import Control.Monad.Reader
import Data.Functor.Identity
import qualified Data.DList as DList
import Data.DList (DList)
import Data.Monoid
import Data.Maybe
import LLVM.NameSupply
import LLVM.Codegen.Operand
import LLVM.Codegen.Type
import LLVM.Codegen.IR
import LLVM.Pretty


data BasicBlock
  = BB
  { bbName :: Name
  , bbInstructions :: DList (Operand, IR)
  , bbTerminator :: Terminator
  } deriving Show

data PartialBlock
  = PartialBlock
  { pbName :: Name
  , pbInstructions :: DList (Operand, IR)
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
  addInstrToCurrentBlock operand
  pure operand
  where
    addInstrToCurrentBlock operand =
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

-- switch :: Monad m => _ -> IRBuilderT m ()
-- switch = _

instance Pretty BasicBlock where
  pretty (BB (Name name) stmts (Terminator term)) =
    let prettyStmts = indent 2 $ vsep $ (map (uncurry prettyStmt) $ DList.apply stmts []) ++ [pretty term]
     in vsep [ pretty name <> ":", prettyStmts ]
    where
      prettyStmt operand instr =
        pretty operand <+> "=" <+> pretty instr

