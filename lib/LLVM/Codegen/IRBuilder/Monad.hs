{-# LANGUAGE TypeFamilies #-}

module LLVM.Codegen.IRBuilder.Monad
  ( module LLVM.Codegen.IRBuilder.Monad
  ) where

-- NOTE: this module only exists to solve a cyclic import

import Prelude hiding (and)
import Control.Monad.State.Lazy (StateT(..), MonadState, modify, execStateT)
import qualified Control.Monad.State.Strict as StrictState
import qualified Control.Monad.State.Lazy as LazyState
import qualified Control.Monad.RWS.Lazy as LazyRWS
import qualified Control.Monad.RWS.Strict as StrictRWS
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Data.Functor.Identity
import qualified Data.DList as DList
import Data.DList (DList)
import Data.Maybe
import LLVM.Codegen.NameSupply
import LLVM.Codegen.Operand
import LLVM.Codegen.IR
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
  , currentBlock :: PartialBlock
  }

newtype IRBuilderT m a
  = IRBuilderT (StateT IRBuilderState (NameSupplyT m) a)
  deriving ( Functor, Applicative, Monad, MonadFix, MonadIO
           , MonadReader (Maybe Name), MonadState IRBuilderState
           , MonadNameSupply
           )
  via StateT IRBuilderState (NameSupplyT m)

instance MonadTrans IRBuilderT where
  lift = IRBuilderT . lift . lift

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

class Monad m => MonadIRBuilder m where
  modifyIRBuilderState :: (IRBuilderState -> IRBuilderState) -> m ()

  default modifyIRBuilderState
    :: (MonadTrans t, MonadIRBuilder m1, m ~ t m1)
    => (IRBuilderState -> IRBuilderState)
    -> m ()
  modifyIRBuilderState = lift . modifyIRBuilderState

instance Monad m => MonadIRBuilder (IRBuilderT m) where
  modifyIRBuilderState = modify

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

