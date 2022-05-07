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
  , ret
  ) where

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
  emitInstr (typeOf lhs) $ Add lhs rhs

ret :: Monad m => Operand -> IRBuilderT m ()
ret val =
  emitTerminator (Terminator (Ret (Just val)))

