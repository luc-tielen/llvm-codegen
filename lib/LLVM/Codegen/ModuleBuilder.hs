module LLVM.Codegen.ModuleBuilder
  ( ModuleBuilderT
  , ModuleBuilder
  , runModuleBuilderT
  , runModuleBuilder
  , MonadModuleBuilder
  , Module(..)
  , Definition(..)
  , function
  , global
  ) where

import Control.Monad.State
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.Text as T
import Data.Functor.Identity
import LLVM.Codegen.IRBuilder
import LLVM.Codegen.Operand
import LLVM.Codegen.Type
import LLVM.NameSupply
import LLVM.Pretty


data Module
  = Module [Definition]

instance Pretty Module where
  pretty (Module defs) =
    vsep $ map pretty defs

data Global
  = GlobalVariable Name Type Constant
  | Function Name Type [Type] [BasicBlock]
  deriving Show

data Definition
  = GlobalDefinition Global
  deriving Show

instance Pretty Definition where
 pretty = \case
   GlobalDefinition g ->
     pretty g

instance Pretty Global where
  pretty = \case
    GlobalVariable name ty constant ->
      "@" <> pretty name <+> "=" <+> "global" <+> pretty ty <+> pretty constant
    Function name retTy argTys body ->
      "define external ccc" <+> pretty retTy <+> fnName <> tupled (zipWith prettyArg [0..] argTys) <+>
        "{" <> hardline <>
        prettyBody body <> hardline <>
        "}"
      where
        fnName = "@" <> pretty name
        prettyArg :: Int -> Type -> Doc ann
        prettyArg i argTy = pretty $ LocalRef argTy $ Name $ T.pack (show i)
        prettyBody blocks = vsep $ map pretty blocks

type ModuleBuilderState = DList Definition

newtype ModuleBuilderT m a
  = ModuleBuilder (StateT ModuleBuilderState m a)
  deriving (Functor, Applicative, Monad, MonadState ModuleBuilderState, MonadFix)
  via StateT ModuleBuilderState m

type ModuleBuilder = ModuleBuilderT Identity

type MonadModuleBuilder m = (MonadState ModuleBuilderState m, MonadFix m)

runModuleBuilderT :: Monad m => ModuleBuilderT m a -> m Module
runModuleBuilderT (ModuleBuilder m) =
  Module . DList.toList <$> execStateT m mempty

runModuleBuilder :: ModuleBuilder a -> Module
runModuleBuilder = runIdentity . runModuleBuilderT


function :: MonadModuleBuilder m => Name -> [Type] -> Type -> ([Operand] -> IRBuilderT m a) -> m Operand
function name tys retTy fnBody = do
  instrs <- runIRBuilderT $ do
    operands <- traverse mkOperand tys
    fnBody operands
  emitDefinition $ GlobalDefinition $ Function name retTy tys instrs
  pure $ ConstantOperand $ GlobalRef (ptr (FunctionType retTy tys)) name

emitDefinition :: MonadModuleBuilder m => Definition -> m ()
emitDefinition def =
  modify $ (flip DList.snoc def)

global :: MonadModuleBuilder m => Name -> Type -> Constant -> m Operand
global name ty constant = do
  emitDefinition $ GlobalDefinition $ GlobalVariable name ty constant
  pure $ ConstantOperand $ GlobalRef (ptr ty) name

-- TODO add extra variant for opaque typedef (if needed)

-- NOTE: Only used internally, this creates an unassigned operand
mkOperand :: Monad m => Type -> IRBuilderT m Operand
mkOperand ty = do
  name <- fresh
  pure $ LocalRef ty name

