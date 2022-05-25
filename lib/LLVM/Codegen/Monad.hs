module LLVM.Codegen.IRBuilder.Monad
  ( module LLVM.Codegen.IRBuilder.Monad
  ) where

-- NOTE: this module only exists to solve a cyclic import

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
import LLVM.Codegen.IR


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
