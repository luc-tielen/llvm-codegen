{-# LANGUAGE TypeFamilies #-}

module LLVM.Codegen.IRBuilder.Monad
  ( IRBuilderT
  , IRBuilder
  , runIRBuilderT
  , runIRBuilder
  , MonadIRBuilder(..)
  , BasicBlock(..)
  , block
  , emitBlockStart
  , named
  , emitInstr
  , emitInstrVoid
  , emitTerminator
  ) where

-- NOTE: this module only exists to solve a cyclic import

import Prelude hiding (and)
import Control.Arrow hiding ((<+>))
import Control.Monad.State.Lazy (StateT(..), MonadState, modify)
import qualified Control.Monad.State.Strict as StrictState
import qualified Control.Monad.State.Lazy as LazyState
import qualified Control.Monad.RWS.Lazy as LazyRWS
import qualified Control.Monad.RWS.Strict as StrictRWS
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Morph
import Data.Functor.Identity
import qualified Data.DList as DList
import Data.DList (DList)
import Data.Maybe
import LLVM.Codegen.NameSupply
import LLVM.Codegen.Operand
import LLVM.Codegen.IR
import LLVM.Codegen.Type
import LLVM.Pretty


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
  , currentPartialBlock :: PartialBlock
  }

newtype IRBuilderT m a
  = IRBuilderT { unIRBuilderT :: (StateT IRBuilderState (NameSupplyT m) a) }
  deriving ( Functor, Applicative, Monad, MonadFix, MonadIO
           , MonadReader (Maybe Name), MonadState IRBuilderState
           , MonadNameSupply
           )
  via StateT IRBuilderState (NameSupplyT m)

instance MonadTrans IRBuilderT where
  lift = IRBuilderT . lift . lift

instance MFunctor IRBuilderT where
  hoist nat = IRBuilderT . hoist (hoist nat) . unIRBuilderT

type IRBuilder = IRBuilderT Identity

runIRBuilderT :: Monad m => IRBuilderT m a -> m (a, [BasicBlock])
runIRBuilderT (IRBuilderT m) = do
  let partialBlock = PartialBlock (Name "start") mempty mempty
      result = runNameSupplyT $ runStateT m (IRBuilderState mempty partialBlock)
  fmap (second getBlocks) result
  where
    getBlocks irState =
      let previousBlocks = DList.apply (basicBlocks irState) mempty
          currentBlk = currentPartialBlock irState
       in previousBlocks ++ [partialBlockToBasicBlock currentBlk]

runIRBuilder :: IRBuilder a -> (a, [BasicBlock])
runIRBuilder = runIdentity . runIRBuilderT

partialBlockToBasicBlock :: PartialBlock -> BasicBlock
partialBlockToBasicBlock pb =
  let currentTerm = fromMaybe (Terminator $ Ret Nothing) $ getFirst $ pbTerminator pb
  in BB (pbName pb) (pbInstructions pb) currentTerm

block :: (MonadNameSupply m, MonadIRBuilder m) => m Name
block = do
  blockName <- getSuggestion >>= \case
    Nothing -> do
      fresh `named` "block"
    Just _sugg ->
      fresh

  emitBlockStart blockName
  pure blockName

emitBlockStart :: (MonadNameSupply m, MonadIRBuilder m) => Name -> m ()
emitBlockStart blockName =
  modifyIRBuilderState $ \s ->
    let currBlock = currentPartialBlock s
        hasntStartedBlock = null (DList.toList (pbInstructions currBlock)) && isNothing (getFirst (pbTerminator currBlock))
        blocks = basicBlocks s
     in s { basicBlocks =
              if hasntStartedBlock
                then blocks
                else DList.snoc blocks (partialBlockToBasicBlock currBlock)
          , currentPartialBlock = PartialBlock blockName mempty mempty
          }

-- NOTE: Only used internally, this creates an unassigned operand
mkOperand :: (MonadNameSupply m, MonadIRBuilder m) => Type -> m Operand
mkOperand ty = do
  name <- fresh
  pure $ LocalRef ty name

emitInstr :: (MonadNameSupply m, MonadIRBuilder m) => Type -> IR -> m Operand
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
    blk { pbTerminator = pbTerminator blk <> First (Just term) }

modifyCurrentBlock :: MonadIRBuilder m => (PartialBlock -> PartialBlock) -> m ()
modifyCurrentBlock f =
  modifyIRBuilderState $ \s ->
    s { currentPartialBlock = f (currentPartialBlock s) }


class Monad m => MonadIRBuilder m where
  modifyIRBuilderState :: (IRBuilderState -> IRBuilderState) -> m ()

  currentBlock :: m Name

  default modifyIRBuilderState
    :: (MonadTrans t, MonadIRBuilder m1, m ~ t m1)
    => (IRBuilderState -> IRBuilderState)
    -> m ()
  modifyIRBuilderState = lift . modifyIRBuilderState

  default currentBlock
    :: (MonadTrans t, MonadIRBuilder m1, m ~ t m1)
    => m Name
  currentBlock =
    lift currentBlock

instance Monad m => MonadIRBuilder (IRBuilderT m) where
  modifyIRBuilderState = modify
  currentBlock =
    LazyState.gets (pbName . currentPartialBlock)

instance MonadIRBuilder m => MonadIRBuilder (StrictState.StateT s m)
instance MonadIRBuilder m => MonadIRBuilder (LazyState.StateT s m)
instance (MonadIRBuilder m, Monoid w) => MonadIRBuilder (StrictRWS.RWST r w s m)
instance (MonadIRBuilder m, Monoid w) => MonadIRBuilder (LazyRWS.RWST r w s m)
instance MonadIRBuilder m => MonadIRBuilder (ReaderT r m)
instance (MonadIRBuilder m, Monoid w) => MonadIRBuilder (WriterT w m)
instance MonadIRBuilder m => MonadIRBuilder (ExceptT e m)

instance Pretty BasicBlock where
  pretty (BB (Name name) stmts (Terminator term)) =
    let prettyStmts = indent 2 $ vsep $ (map (uncurry prettyStmt) $ DList.apply stmts []) ++ [pretty term]
     in vsep [ pretty name <> ":", prettyStmts ]
    where
      prettyStmt operand instr =
        let instrDoc = pretty instr
         in maybe instrDoc (\op -> pretty op <+> "=" <+> instrDoc) operand

