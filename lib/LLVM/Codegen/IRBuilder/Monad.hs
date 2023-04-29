{-# LANGUAGE TypeFamilies, RankNTypes, MultiParamTypeClasses, UndecidableInstances, BangPatterns, TypeOperators #-}

module LLVM.Codegen.IRBuilder.Monad
  ( IRBuilderT
  , IRBuilder
  , runIRBuilderT
  , runIRBuilder
  , MonadIRBuilder(..)
  , BasicBlock(..)
  , block
  , blockNamed
  , emitBlockStart
  , emitInstr
  , emitInstrVoid
  , emitTerminator
  , renderBasicBlock
  , freshName
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
import qualified Data.Text as T
import qualified Data.DList as DList
import Data.DList (DList)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import LLVM.Codegen.Operand
import LLVM.Codegen.IR
import LLVM.Codegen.Type
import LLVM.Codegen.Name
import LLVM.Pretty


data BasicBlock
  = BB
  { bbName :: !Name
  , bbInstructions :: !(DList (Maybe Operand, IR))
  , bbTerminator :: !Terminator
  } deriving Show

data PartialBlock
  = PartialBlock
  { pbName :: !Name
  , pbInstructions :: !(DList (Maybe Operand, IR))
  , pbTerminator :: !(First Terminator)
  , pbNumInstrs :: !Int
  }

data IRBuilderState
  = IRBuilderState
  { allocas :: !(DList (Maybe Operand, IR))
  , basicBlocks :: !(DList BasicBlock)
  , currentPartialBlock :: !PartialBlock
  , operandCounter :: !Int
  , nameMap :: !(Map T.Text Int)
  }

newtype IRBuilderT m a
  = IRBuilderT { unIRBuilderT :: StateT IRBuilderState m a }
  deriving ( Functor, Applicative, Monad, MonadFix, MonadIO
           , MonadError e
           )
  via StateT IRBuilderState m

instance MonadReader r m => MonadReader r (IRBuilderT m) where
  ask = lift ask
  {-# INLINEABLE ask #-}
  local = mapIRBuilderT . local
  {-# INLINEABLE local #-}

-- TODO MonadWriter

mapIRBuilderT :: (Monad m, Monad n) => (m a -> n a) -> IRBuilderT m a -> IRBuilderT n a
mapIRBuilderT f (IRBuilderT inner) =
  IRBuilderT $ do
    s <- LazyState.get
    LazyState.mapStateT (g s) inner
  where
    g s = fmap (,s) . f . fmap fst
{-# INLINEABLE mapIRBuilderT #-}

instance MonadState s m => MonadState s (IRBuilderT m) where
  state = lift . StrictState.state
  {-# INLINEABLE state #-}

instance MonadTrans IRBuilderT where
  lift = IRBuilderT . lift
  {-# INLINEABLE lift #-}

instance MFunctor IRBuilderT where
  hoist nat = IRBuilderT . hoist nat . unIRBuilderT
  {-# INLINEABLE hoist #-}

type IRBuilder = IRBuilderT Identity

runIRBuilderT :: Monad m => IRBuilderT m a -> m (a, [BasicBlock])
runIRBuilderT (IRBuilderT m) = do
  let partialBlock = PartialBlock (Name "start") mempty mempty 0
      result = runStateT m (IRBuilderState mempty mempty partialBlock 0 mempty)
  fmap (second getBlocks) result
  where
    getBlocks irState =
      case blocks of
        [] -> []
        (firstBlk:restBlks) ->
          let firstBlk' = firstBlk { bbInstructions = DList.append allocations (bbInstructions firstBlk) }
           in (firstBlk':restBlks)
      where
        previousBlocks = DList.apply (basicBlocks irState) mempty
        currentBlk = currentPartialBlock irState
        blocks = previousBlocks <> [partialBlockToBasicBlock currentBlk]
        allocations = allocas irState
{-# INLINEABLE runIRBuilderT #-}

runIRBuilder :: IRBuilder a -> (a, [BasicBlock])
runIRBuilder = runIdentity . runIRBuilderT
{-# INLINEABLE runIRBuilder #-}

partialBlockToBasicBlock :: PartialBlock -> BasicBlock
partialBlockToBasicBlock pb =
  let currentTerm = fromMaybe (Terminator $ Ret Nothing) $ getFirst $ pbTerminator pb
  in BB (pbName pb) (pbInstructions pb) currentTerm
{-# INLINEABLE partialBlockToBasicBlock #-}

block :: (MonadIRBuilder m) => m Name
block = do
  blockName <- freshName (Just "block")
  emitBlockStart blockName
  pure blockName
{-# INLINEABLE block #-}

blockNamed :: (MonadIRBuilder m) => T.Text -> m Name
blockNamed blkName = do
  blockName <- freshName (Just blkName)
  emitBlockStart blockName
  pure blockName
{-# INLINEABLE blockNamed #-}

emitBlockStart :: (MonadIRBuilder m) => Name -> m ()
emitBlockStart blockName =
  modifyIRBuilderState $ \s ->
    let currBlock = currentPartialBlock s
        hasntStartedBlock = (pbNumInstrs currBlock == 0) && isNothing (getFirst (pbTerminator currBlock))
        blocks = basicBlocks s
        -- If the current block is empty:
        --   Insert a dummy basic block that jumps directly to the next block, to avoid continuity errors.
        --   Normally, LLVM should optimize this away since it is semantically a no-op.
        -- Otherwise:
        --   Append the current block to the existing list of blocks.
        --
        -- NOTE: This is different behavior compared to the llvm-hs-pure library,
        -- but this avoids a lot of partial functions!
        newBlock =
          if hasntStartedBlock
            then BB (pbName currBlock) mempty (Terminator $ Br blockName)
            else partialBlockToBasicBlock currBlock
     in s { basicBlocks = DList.snoc blocks newBlock
          , currentPartialBlock = PartialBlock blockName mempty mempty 0
          }
{-# INLINEABLE emitBlockStart #-}

-- NOTE: Only used internally, this creates an unassigned operand
mkOperand :: (MonadIRBuilder m) => Type -> m Operand
mkOperand ty = LocalRef ty <$!> freshUnnamed
{-# INLINEABLE mkOperand #-}

freshName :: MonadIRBuilder m => Maybe T.Text -> m Name
freshName = \case
  Nothing -> freshUnnamed
  Just suggestion -> do
    nameMapping <- nameMap <$> getIRBuilderState
    let !mCount = M.lookup suggestion nameMapping
        !count = fromMaybe 0 mCount
        !newMapping = M.insert suggestion (count + 1) nameMapping
    modifyIRBuilderState $ \s -> s { nameMap = newMapping }
    pure $! Name $! suggestion <> "_" <> T.pack (show count)
{-# INLINEABLE freshName #-}

freshUnnamed :: MonadIRBuilder m => m Name
freshUnnamed = do
  !ctr <- operandCounter <$> getIRBuilderState
  let !newCount = ctr + 1
  modifyIRBuilderState $ \s -> s { operandCounter = newCount }
  pure $! Generated ctr
{-# INLINEABLE freshUnnamed #-}

emitInstr :: (MonadIRBuilder m) => Type -> IR -> m Operand
emitInstr ty = \case
  instr@(Alloca {}) -> do
    -- For performant code, all alloca instructions should be at the start of the function!
    -- https://llvm.org/docs/Frontend/PerformanceTips.html#use-of-allocas
    -- (A custom operand name is only used here to avoid having to re-number all operands.)
    operand <- LocalRef ty <$!> freshName (Just "stack.ptr")
    addAlloca operand instr
    pure operand
  instr -> do
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
  modifyCurrentBlock $ \blk ->
    let instrs = DList.snoc (pbInstructions blk) (operand, instr)
        in blk { pbInstructions = instrs, pbNumInstrs = pbNumInstrs blk + 1 }
{-# INLINEABLE addInstrToCurrentBlock #-}

addAlloca :: MonadIRBuilder m => Operand -> IR -> m ()
addAlloca operand instr =
  modifyIRBuilderState $ \s ->
    s { allocas = DList.snoc (allocas s) (Just operand, instr) }
{-# INLINEABLE addAlloca #-}

emitTerminator :: MonadIRBuilder m => Terminator -> m ()
emitTerminator term =
  modifyCurrentBlock $ \blk ->
    blk { pbTerminator = pbTerminator blk <> First (Just term) }
{-# INLINABLE emitTerminator #-}

modifyCurrentBlock :: MonadIRBuilder m => (PartialBlock -> PartialBlock) -> m ()
modifyCurrentBlock f =
  modifyIRBuilderState $ \s ->
    s { currentPartialBlock = f (currentPartialBlock s) }
{-# INLINEABLE modifyCurrentBlock #-}

class Monad m => MonadIRBuilder m where
  getIRBuilderState :: m IRBuilderState

  modifyIRBuilderState :: (IRBuilderState -> IRBuilderState) -> m ()

  currentBlock :: m Name

  default getIRBuilderState
    :: (MonadTrans t, MonadIRBuilder m1, m ~ t m1)
    => m IRBuilderState
  getIRBuilderState = lift getIRBuilderState
  {-# INLINEABLE getIRBuilderState #-}

  default modifyIRBuilderState
    :: (MonadTrans t, MonadIRBuilder m1, m ~ t m1)
    => (IRBuilderState -> IRBuilderState)
    -> m ()
  modifyIRBuilderState = lift . modifyIRBuilderState
  {-# INLINEABLE modifyIRBuilderState #-}

  default currentBlock
    :: (MonadTrans t, MonadIRBuilder m1, m ~ t m1)
    => m Name
  currentBlock = lift currentBlock
  {-# INLINEABLE currentBlock #-}

instance Monad m => MonadIRBuilder (IRBuilderT m) where
  getIRBuilderState = IRBuilderT LazyState.get
  {-# INLINEABLE getIRBuilderState #-}

  modifyIRBuilderState = IRBuilderT . modify
  {-# INLINEABLE modifyIRBuilderState #-}

  currentBlock =
    IRBuilderT $ LazyState.gets (pbName . currentPartialBlock)
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
    stmts' = DList.apply stmts []
    renderStmt :: Buffer %1 -> (Maybe Operand, IR) -> Buffer
    renderStmt buf' (mOperand, instr) =
      withIndent buf' (\buf'' -> renderStmt' buf'' mOperand instr)
    renderStmt' :: Buffer %1 -> Maybe Operand -> IR -> Buffer
    renderStmt' buf' mOperand instr =
      renderMaybe buf' mOperand (\buf'' operand -> buf'' `renderOperand` operand |># " = "#) `renderIR` instr
{-# INLINABLE renderBasicBlock #-}
