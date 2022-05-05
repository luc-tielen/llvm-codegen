{-# LANGUAGE RecursiveDo #-}

module LLVM.Codegen
  ( module LLVM.Codegen  -- TODO clean up exports
  ) where

import Control.Monad.State.Lazy
import Data.Functor.Identity
import qualified Data.Text as T
import Data.Text (Text)

type Counter = Int

data IRBuilderState
  = IRBuilderState
  { operandCounter :: Counter
  , instructions :: [(Operand, IR)]  -- TODO diff list
  }

newtype IRBuilderT m a
  = IRBuilderT (StateT IRBuilderState m a)
  deriving (Functor, Applicative, Monad, MonadState IRBuilderState, MonadFix)
  via StateT IRBuilderState m

type IRBuilder = IRBuilderT Identity

runIRBuilderT :: Monad m => IRBuilderT m a -> m [(Operand, IR)]
runIRBuilderT (IRBuilderT m) =
  -- TODO: need scc?
  fmap (reverse . instructions) <$> execStateT m $ IRBuilderState 0 mempty

runIRBuilder :: IRBuilder a -> [(Operand, IR)]
runIRBuilder = runIdentity . runIRBuilderT

newtype Operand
  = Operand { unOperand :: Text }
  deriving Show  -- TODO remove

newtype Label = Label Text

data IR
  = Add Operand Operand
  deriving Show  -- TODO remove

add :: Monad m => Operand -> Operand -> IRBuilderT m Operand
add lhs rhs = emitInstr $ Add lhs rhs

emitInstr :: Monad m => IR -> IRBuilderT m Operand
emitInstr instr = do
  operand <- mkOperand
  modify $ \(IRBuilderState counter instrs) ->
    IRBuilderState (counter + 1) ((operand, instr) : instrs)
  pure operand

-- NOTE: Only used internally, this creates an unassigned operand
mkOperand :: Monad m => IRBuilderT m Operand
mkOperand = do
  count <- gets operandCounter
  pure $ Operand $ "%" <> T.pack (show count)

example :: [(Operand, IR)]
example = runIRBuilder $ mdo
  let a = Operand "a"
  let b = Operand "b"
  d <- add a c
  c <- add a b
  pure d


-- $> example
