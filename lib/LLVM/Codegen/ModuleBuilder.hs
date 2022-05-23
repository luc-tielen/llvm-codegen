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
  , typedef
  ) where

import Control.Monad.State
import Data.DList (DList)
import Data.Map (Map)
import qualified Data.DList as DList
import qualified Data.Map as Map
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
  | TypeDefinition Name Type
  deriving Show

instance Pretty Definition where
 pretty = \case
   GlobalDefinition g ->
     pretty g
   TypeDefinition name ty ->
     "%" <> pretty name <+> "=" <+> pretty ty

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

data ModuleBuilderState
  = ModuleBuilderState
  { definitions :: DList Definition
  , types :: Map Name Type
  }

newtype ModuleBuilderT m a
  = ModuleBuilder (StateT ModuleBuilderState m a)
  deriving (Functor, Applicative, Monad, MonadState ModuleBuilderState, MonadFix, MonadIO)
  via StateT ModuleBuilderState m

type ModuleBuilder = ModuleBuilderT Identity

type MonadModuleBuilder m = (MonadState ModuleBuilderState m, MonadFix m)

runModuleBuilderT :: Monad m => ModuleBuilderT m a -> m Module
runModuleBuilderT (ModuleBuilder m) =
  Module . DList.toList . definitions <$> execStateT m beginState
  where
    beginState = ModuleBuilderState mempty mempty

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
  modify $ \s -> s { definitions = DList.snoc (definitions s) def }

addType :: MonadModuleBuilder m => Name -> Type -> m ()
addType name ty =
  modify $ \s -> s { types = Map.insert name ty (types s) }

global :: MonadModuleBuilder m => Name -> Type -> Constant -> m Operand
global name ty constant = do
  emitDefinition $ GlobalDefinition $ GlobalVariable name ty constant
  pure $ ConstantOperand $ GlobalRef (ptr ty) name

typedef :: MonadModuleBuilder m => Name -> Type -> m Type
typedef name ty = do
  emitDefinition $ TypeDefinition name ty
  addType name ty
  pure $ NamedTypeReference name

-- TODO add extra variant for opaque (no type provided) typedef (if needed)

-- NOTE: Only used internally, this creates an unassigned operand
mkOperand :: Monad m => Type -> IRBuilderT m Operand
mkOperand ty = do
  name <- fresh
  pure $ LocalRef ty name

