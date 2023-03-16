{-# LANGUAGE TypeFamilies, RankNTypes, MultiParamTypeClasses, UndecidableInstances, UnboxedTuples #-}

module LLVM.Codegen.IRBuilder.Monad
  ( IRBuilderT
  , IRBuilder
  , runIRBuilderT
  , MonadIRBuilder(..)
  , BasicBlock(..)
  , block
  , emitBlockStart
  , named
  , emitInstr
  , emitInstrVoid
  , emitTerminator
  , renderBasicBlock
  ) where

-- NOTE: this module only exists to solve a cyclic import

import Prelude hiding (and)
import Control.Monad.State.Lazy (MonadState)
import qualified Control.Monad.State.Strict as StrictState
import qualified Control.Monad.State.Lazy as LazyState
import qualified Control.Monad.RWS.Lazy as LazyRWS
import qualified Control.Monad.RWS.Strict as StrictRWS
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Morph
import Data.Functor.Identity
import qualified Data.Vector as V
import Data.Maybe
import Data.IORef
import LLVM.Codegen.NameSupply
import LLVM.Codegen.Operand
import LLVM.Codegen.IR
import LLVM.Codegen.Type
import LLVM.Codegen.Name
import qualified LLVM.Codegen.ArrayList as L
import LLVM.Pretty


data BasicBlock
  = BB
  { bbName :: !Name
  , bbInstructions :: !(V.Vector (Maybe Operand, IR))
  , bbTerminator :: !Terminator
  }

data PartialBlock
  = PartialBlock
  { pbName :: !Name
  , pbInstructions :: !(L.ArrayList (Maybe Operand, IR))
  , pbTerminator :: !(First Terminator)
  }

data IRBuilderState
  = IRBuilderState
  { basicBlocks :: !(L.ArrayList BasicBlock)
  , currentPartialBlock :: !PartialBlock
  }

newtype IRBuilderT m a
  = IRBuilderT { unIRBuilderT :: ReaderT (IORef IRBuilderState) (NameSupplyT m) a }
  deriving ( Functor, Applicative, Monad, MonadFix, MonadIO
           , MonadNameSupply, MonadError e
           )
  via ReaderT (IORef IRBuilderState) (NameSupplyT m)

instance MonadReader r m => MonadReader r (IRBuilderT m) where
  ask = lift ask
  {-# INLINEABLE ask #-}
  local = mapIRBuilderT . local
  {-# INLINEABLE local #-}

-- TODO MonadWriter

mapIRBuilderT :: (Monad m, Monad n) => (m a -> n a) -> IRBuilderT m a -> IRBuilderT n a
mapIRBuilderT f (IRBuilderT inner) =
  IRBuilderT $ mapReaderT (mapNameSupplyT f) inner
{-# INLINEABLE mapIRBuilderT #-}

instance MonadState s m => MonadState s (IRBuilderT m) where
  state = lift . StrictState.state
  {-# INLINEABLE state #-}

instance MonadTrans IRBuilderT where
  lift = IRBuilderT . lift . lift
  {-# INLINEABLE lift #-}

instance MFunctor IRBuilderT where
  hoist nat = IRBuilderT . hoist (hoist nat) . unIRBuilderT
  {-# INLINEABLE hoist #-}

type IRBuilder = IRBuilderT Identity

runIRBuilderT :: MonadIO m => IRBuilderT m a -> m (a, V.Vector BasicBlock)
runIRBuilderT (IRBuilderT m) = do
  pbInstrs <- liftIO $ L.new 10  -- pre-allocate memory for 10 instructions
  let partialBlock = PartialBlock (Name "start") pbInstrs mempty
  ref <- liftIO $ do
    blocks <- L.new 10  -- pre-allocate memory for 10 basic blocks
    newIORef $ IRBuilderState blocks partialBlock
  let result = runNameSupplyT $ runReaderT m ref
  a <- result
  blocks <- liftIO $ getBlocks =<< readIORef ref
  pure (a, blocks)
  where
    getBlocks irState = do
      let currentBlk = currentPartialBlock irState
      newBlock <- partialBlockToBasicBlock currentBlk
      blocks <- L.append newBlock (basicBlocks irState)
      L.toVector blocks
{-# INLINEABLE runIRBuilderT #-}

partialBlockToBasicBlock :: PartialBlock -> IO BasicBlock
partialBlockToBasicBlock pb = do
  instrs <- L.toVector (pbInstructions pb)
  let currentTerm = fromMaybe (Terminator $ Ret Nothing) $ getFirst $ pbTerminator pb
  pure $ BB (pbName pb) instrs currentTerm
{-# INLINEABLE partialBlockToBasicBlock #-}

block :: (MonadNameSupply m, MonadIRBuilder m) => m Name
block = do
  blockName <- getSuggestion >>= \case
    Nothing -> do
      fresh `named` "block"
    Just _sugg ->
      fresh

  emitBlockStart blockName
  pure blockName
{-# INLINEABLE block #-}

emitBlockStart :: (MonadNameSupply m, MonadIRBuilder m) => Name -> m ()
emitBlockStart blockName = do
  ref <- getIRBuilderStateRef
  liftIO $ do
    s <- readIORef ref
    let currBlock = currentPartialBlock s
        hasntStartedBlock = L.isEmpty (pbInstructions currBlock) && isNothing (getFirst (pbTerminator currBlock))
        blocks = basicBlocks s
    -- If the current block is empty:
    --   Insert a dummy basic block that jumps directly to the next block, to avoid continuity errors.
    --   Normally, LLVM should optimize this away since it is semantically a no-op.
    -- Otherwise:
    --   Append the current block to the existing list of blocks.
    --
    -- NOTE: This is different behavior compared to the llvm-hs-pure library,
    -- but this avoids a lot of partial functions!
    newBlock <-
      if hasntStartedBlock
        then pure $ BB (pbName currBlock) mempty (Terminator $ Br blockName)
        else partialBlockToBasicBlock currBlock
    basicBlocks' <- L.append newBlock blocks
    instrList <- L.new 10
    writeIORef ref $
      s { basicBlocks = basicBlocks'
        , currentPartialBlock = PartialBlock blockName instrList mempty
        }
{-# INLINEABLE emitBlockStart #-}

-- NOTE: Only used internally, this creates an unassigned operand
mkOperand :: (MonadNameSupply m, MonadIRBuilder m) => Type -> m Operand
mkOperand ty =
  LocalRef ty <$> fresh

emitInstr :: (MonadNameSupply m, MonadIRBuilder m) => Type -> IR -> m Operand
emitInstr ty instr = do
  operand <- mkOperand ty
  addInstrToCurrentBlock (Just operand) instr
  pure operand
{-# INLINABLE emitInstr #-}

emitInstrVoid :: MonadIRBuilder m => IR -> m ()
emitInstrVoid =
  addInstrToCurrentBlock Nothing
{-# INLINABLE emitInstrVoid #-}

addInstrToCurrentBlock :: MonadIRBuilder m => Maybe Operand -> IR -> m ()
addInstrToCurrentBlock operand instr =
  modifyCurrentBlock $ \blk -> do
    instrs <- L.append (operand, instr) (pbInstructions blk)
    pure $ blk { pbInstructions = instrs }
{-# INLINEABLE addInstrToCurrentBlock #-}

emitTerminator :: MonadIRBuilder m => Terminator -> m ()
emitTerminator term =
  modifyCurrentBlock $ \blk ->
    pure $ blk { pbTerminator = pbTerminator blk <> First (Just term) }
{-# INLINABLE emitTerminator #-}

modifyCurrentBlock :: MonadIRBuilder m => (PartialBlock -> IO PartialBlock) -> m ()
modifyCurrentBlock f = do
  ref <- getIRBuilderStateRef
  liftIO $ do
    s <- readIORef ref
    currentPB <- f (currentPartialBlock s)
    writeIORef ref $ s { currentPartialBlock = currentPB }
{-# INLINEABLE modifyCurrentBlock #-}

class MonadIO m => MonadIRBuilder m where
  getIRBuilderStateRef :: m (IORef IRBuilderState)

  currentBlock :: m Name

  default getIRBuilderStateRef
    :: (MonadTrans t, MonadIRBuilder m1, m ~ t m1)
    => m (IORef IRBuilderState)
  getIRBuilderStateRef = lift getIRBuilderStateRef
  {-# INLINEABLE getIRBuilderStateRef #-}

  default currentBlock
    :: (MonadTrans t, MonadIRBuilder m1, m ~ t m1)
    => m Name
  currentBlock =
    lift currentBlock
  {-# INLINEABLE currentBlock #-}

instance MonadIO m => MonadIRBuilder (IRBuilderT m) where
  getIRBuilderStateRef = IRBuilderT ask
  {-# INLINEABLE getIRBuilderStateRef #-}
  currentBlock = do
    ref <- getIRBuilderStateRef
    liftIO $ pbName . currentPartialBlock <$> readIORef ref
  {-# INLINEABLE currentBlock #-}

instance MonadIRBuilder m => MonadIRBuilder (StrictState.StateT s m)
instance MonadIRBuilder m => MonadIRBuilder (LazyState.StateT s m)
instance (MonadIRBuilder m, Monoid w) => MonadIRBuilder (StrictRWS.RWST r w s m)
instance (MonadIRBuilder m, Monoid w) => MonadIRBuilder (LazyRWS.RWST r w s m)
instance MonadIRBuilder m => MonadIRBuilder (ReaderT r m)
instance (MonadIRBuilder m, Monoid w) => MonadIRBuilder (WriterT w m)
instance MonadIRBuilder m => MonadIRBuilder (ExceptT e m)

renderBasicBlock :: Renderer BasicBlock
renderBasicBlock buf (BB name stmts (Terminator term)) =
  if null stmts
    then (renderName buf name |># ":\n  "#) `renderIR` term
    else (vsep (renderName buf name |># ":\n"#) stmts' renderStmt |># "\n  "#) `renderIR` term
  where
    stmts' = V.toList stmts
    renderStmt :: Buffer %1 -> (Maybe Operand, IR) -> Buffer
    renderStmt buf' (mOperand, instr) =
      withIndent buf' (\buf'' -> renderStmt' buf'' mOperand instr)
    renderStmt' :: Buffer %1 -> Maybe Operand -> IR -> Buffer
    renderStmt' buf' mOperand instr =
      renderMaybe buf' mOperand (\buf'' operand -> buf'' `renderOperand` operand |># " = "#) `renderIR` instr
{-# INLINABLE renderBasicBlock #-}
