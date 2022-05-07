module LLVM.Codegen.ModuleBuilder
  ( ModuleBuilderT
  , ModuleBuilder
  , runModuleBuilderT
  , runModuleBuilder
  , Definition(..)
  , function
  ) where

import Control.Monad.State
import Data.Functor.Identity
import LLVM.Codegen.IRBuilder
import LLVM.Codegen.Operand
import LLVM.Codegen.Type
import LLVM.NameSupply


data Definition = Function Name Type [Type] [BasicBlock]
  deriving Show

type ModuleBuilderState = [Definition]

newtype ModuleBuilderT m a
  = ModuleBuilder (StateT ModuleBuilderState m a)
  deriving (Functor, Applicative, Monad, MonadState ModuleBuilderState, MonadFix)
  via StateT ModuleBuilderState m

type ModuleBuilder = ModuleBuilderT Identity

runModuleBuilderT :: Monad m => ModuleBuilderT m a -> m [Definition]
runModuleBuilderT (ModuleBuilder m) =
  execStateT m []

runModuleBuilder :: ModuleBuilder a -> [Definition]
runModuleBuilder = runIdentity . runModuleBuilderT


function :: Monad m => Name -> [Type] -> Type -> ([Operand] -> IRBuilderT (ModuleBuilderT m) a) -> ModuleBuilderT m Operand
function name tys retTy fnBody = do
  instrs <- runIRBuilderT $ do
    operands <- traverse mkOperand tys
    fnBody operands
  modify $ (Function name retTy tys instrs:)
  pure $ GlobalRef (PointerType (FunctionType retTy tys)) name

-- NOTE: Only used internally, this creates an unassigned operand
mkOperand :: Monad m => Type -> IRBuilderT m Operand
mkOperand ty = do
  name <- fresh
  pure $ LocalRef ty name

