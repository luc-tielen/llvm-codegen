{-# LANGUAGE TypeFamilies #-}

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
  , lookupType
  ) where

import Control.Monad.State.Lazy (StateT(..), MonadState, State, execStateT, modify, gets)
import qualified Control.Monad.State.Strict as StrictState
import qualified Control.Monad.State.Lazy as LazyState
import qualified Control.Monad.RWS.Lazy as LazyRWS
import qualified Control.Monad.RWS.Strict as StrictRWS
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Morph
import Data.DList (DList)
import Data.Map (Map)
import qualified Data.DList as DList
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.List as L
import Data.Functor.Identity
import LLVM.Codegen.IRBuilder.Monad
import LLVM.Codegen.Operand
import LLVM.Codegen.Type
import LLVM.Codegen.NameSupply
import LLVM.Pretty


data Module
  = Module [Definition]

instance Pretty Module where
  pretty (Module defs) =
    vsep $ L.intersperse mempty $ map pretty defs

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
     "%" <> pretty name <+> "=" <+> "type" <+> pretty ty

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
        prettyArg i argTy = pretty argTy <+> pretty (LocalRef argTy $ Name $ T.pack $ show i)
        prettyBody blocks = vsep $ map pretty blocks

data ModuleBuilderState
  = ModuleBuilderState
  { definitions :: DList Definition
  , types :: Map Name Type
  }

newtype ModuleBuilderT m a
  = ModuleBuilderT { unModuleBuilderT :: (StateT ModuleBuilderState m a) }
  deriving (Functor, Applicative, Monad, MonadState ModuleBuilderState, MonadFix, MonadIO)
  via StateT ModuleBuilderState m

type ModuleBuilder = ModuleBuilderT Identity

instance MFunctor ModuleBuilderT where
  hoist nat = ModuleBuilderT . hoist nat . unModuleBuilderT

class Monad m => MonadModuleBuilder m where
  liftModuleBuilderState :: State ModuleBuilderState a -> m a

  default liftModuleBuilderState
    :: (MonadTrans t, MonadModuleBuilder m1, m ~ t m1)
    => State ModuleBuilderState a
    -> m a
  liftModuleBuilderState = lift . liftModuleBuilderState

instance Monad m => MonadModuleBuilder (ModuleBuilderT m) where
  liftModuleBuilderState (StateT s) =
    ModuleBuilderT $ StateT $ pure . runIdentity . s

instance MonadModuleBuilder m => MonadModuleBuilder (IRBuilderT m)
instance MonadModuleBuilder m => MonadModuleBuilder (StrictState.StateT s m)
instance MonadModuleBuilder m => MonadModuleBuilder (LazyState.StateT s m)
instance (MonadModuleBuilder m, Monoid w) => MonadModuleBuilder (StrictRWS.RWST r w s m)
instance (MonadModuleBuilder m, Monoid w) => MonadModuleBuilder (LazyRWS.RWST r w s m)
instance MonadModuleBuilder m => MonadModuleBuilder (ReaderT r m)
instance (MonadModuleBuilder m, Monoid w) => MonadModuleBuilder (WriterT w m)
instance MonadModuleBuilder m => MonadModuleBuilder (ExceptT e m)

runModuleBuilderT :: Monad m => ModuleBuilderT m a -> m Module
runModuleBuilderT (ModuleBuilderT m) =
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
  liftModuleBuilderState $ modify $ \s -> s { definitions = DList.snoc (definitions s) def }

lookupType :: MonadModuleBuilder m => Name -> m (Maybe Type)
lookupType name =
  liftModuleBuilderState $ gets (Map.lookup name . types)

addType :: MonadModuleBuilder m => Name -> Type -> m ()
addType name ty =
  liftModuleBuilderState $ modify $ \s -> s { types = Map.insert name ty (types s) }

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

