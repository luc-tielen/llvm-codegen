{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, UndecidableInstances #-}

module LLVM.Codegen.ModuleBuilder
  ( ModuleBuilderT
  , ModuleBuilder
  , runModuleBuilderT
  , runModuleBuilder
  , MonadModuleBuilder
  , Module(..)
  , Definition(..)
  , ParameterName(..)
  , FunctionAttribute(..)
  , function
  , global
  , globalUtf8StringPtr
  , extern
  , typedef
  , opaqueTypedef
  , getTypedefs
  , lookupType
  , withDefaultFunctionAttributes
  ) where

import GHC.Stack
import Control.Monad.State.Lazy (StateT(..), MonadState, State, execStateT, modify, gets, get, put)
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
import Data.String
import qualified Data.DList as DList
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Functor.Identity
import LLVM.Codegen.IRBuilder.Monad
import LLVM.Codegen.Operand
import LLVM.Codegen.Type
import LLVM.Codegen.Flag
import LLVM.Codegen.NameSupply
import LLVM.Codegen.IR
import LLVM.Pretty

newtype Module
  = Module [Definition]

instance Pretty Module where
  pretty (Module defs) =
    vsep $ L.intersperse mempty $ map pretty defs

data ParameterName
  = ParameterName T.Text
  | NoParameterName
  deriving Show

instance IsString ParameterName where
  fromString = ParameterName . fromString

data FunctionAttribute
  = WasmExportName T.Text
  | AlwaysInline
  -- Add more as needed..
  deriving Show

data Global
  = GlobalVariable Name Type Constant
  | Function Name Type [(Type, ParameterName)] [FunctionAttribute] [BasicBlock]
  deriving Show

data Typedef
  = Opaque
  | Clear Type
  deriving Show

data Definition
  = GlobalDefinition Global
  | TypeDefinition Name Typedef
  deriving Show

instance Pretty Definition where
 pretty = \case
   GlobalDefinition g ->
     pretty g
   TypeDefinition name typeDef ->
     let prettyTy = case typeDef of
          Opaque -> "opaque"
          Clear ty -> pretty ty
      in "%" <> pretty name <+> "=" <+> "type" <+> prettyTy

instance Pretty Global where
  pretty = \case
    GlobalVariable name ty constant ->
      "@" <> pretty name <+> "=" <+> "global" <+> pretty ty <+> pretty constant
    Function name retTy args attrs body
      | null body ->
        "declare external ccc" <+> pretty retTy <+> fnName <> toTuple (map (pretty . fst) args) <> prettyAttrs
      | otherwise ->
        "define external ccc" <+> pretty retTy <+> fnName <> toTuple (zipWith prettyArg [0..] args) <> prettyAttrs <+>
          "{" <> hardline <>
          prettyBody body <> hardline <>
          "}"
      where
        fnName = "@" <> pretty name
        prettyArg :: Int -> (Type, ParameterName) -> Doc ann
        prettyArg i (argTy, nm) =
          case nm of
            NoParameterName ->
              pretty argTy <+> pretty (LocalRef argTy $ Name $ T.pack $ show i)
            ParameterName paramName ->
              pretty argTy <+> pretty (LocalRef argTy $ Name paramName)
        prettyBody blocks =
          vsep $ map pretty blocks
        prettyAttrs =
          if null attrs
            then mempty
            else mempty <+> hsep (map pretty attrs)
        toTuple argDocs =
          parens $ argDocs `sepBy` ", "
        sepBy docs separator =
          mconcat $ L.intersperse separator docs

instance Pretty FunctionAttribute where
  pretty = \case
    AlwaysInline ->
      "alwaysinline"
    WasmExportName name ->
      dquotes "wasm-export-name" <> "=" <> dquotes (pretty name)

data ModuleBuilderState
  = ModuleBuilderState
  { definitions :: DList Definition
  , types :: Map Name Type
  , defaultFunctionAttributes :: [FunctionAttribute]
  }

newtype ModuleBuilderT m a
  = ModuleBuilderT { unModuleBuilderT :: StateT ModuleBuilderState m a }
  deriving ( Functor, Applicative, Monad, MonadFix, MonadIO
           , MonadError e
           )
  via StateT ModuleBuilderState m

type ModuleBuilder = ModuleBuilderT Identity

instance MonadTrans ModuleBuilderT where
  lift = ModuleBuilderT . lift

instance MonadReader r m => MonadReader r (ModuleBuilderT m) where
  ask = lift ask
  local = mapModuleBuilderT . local

mapModuleBuilderT :: (Functor m, Monad n) => (m a -> n a) -> ModuleBuilderT m a -> ModuleBuilderT n a
mapModuleBuilderT f (ModuleBuilderT inner) =
  ModuleBuilderT $ do
    s <- LazyState.get
    LazyState.mapStateT (g s) inner
  where
    g s = fmap (,s) . f . fmap fst

instance MonadState s m => MonadState s (ModuleBuilderT m) where
  state = lift . LazyState.state

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
    beginState = ModuleBuilderState mempty mempty []

withDefaultFunctionAttributes
  :: MonadModuleBuilder m
  => ([FunctionAttribute] -> [FunctionAttribute])
  -> m a -> m a
withDefaultFunctionAttributes f m = do
  fnAttrs <- liftModuleBuilderState (gets defaultFunctionAttributes)
  liftModuleBuilderState $
    modify $ \s -> s { defaultFunctionAttributes = f fnAttrs }
  result <- m
  liftModuleBuilderState $
    modify $ \s -> s { defaultFunctionAttributes = fnAttrs }
  pure result

getDefaultFunctionAttributes :: MonadModuleBuilder m => m [FunctionAttribute]
getDefaultFunctionAttributes =
  liftModuleBuilderState $ gets defaultFunctionAttributes

runModuleBuilder :: ModuleBuilder a -> Module
runModuleBuilder = runIdentity . runModuleBuilderT

function :: (HasCallStack, MonadModuleBuilder m)
         => Name -> [(Type, ParameterName)] -> Type -> ([Operand] -> IRBuilderT m a) -> m Operand
function name args retTy fnBody = do
  (names, instrs) <- runIRBuilderT $ do
    (names, operands) <- unzip <$> traverse (uncurry mkOperand) args
    _ <- fnBody operands
    pure names
  let args' = zipWith (\argName (ty, _) -> (ty, ParameterName $ unName argName)) names args
  fnAttrs <- getDefaultFunctionAttributes
  emitDefinition $ GlobalDefinition $ Function name retTy args' fnAttrs instrs
  pure $ ConstantOperand $ GlobalRef (ptr (FunctionType retTy $ map fst args)) name

emitDefinition :: MonadModuleBuilder m => Definition -> m ()
emitDefinition def =
  liftModuleBuilderState $ modify $ \s -> s { definitions = DList.snoc (definitions s) def }

getTypedefs :: MonadModuleBuilder m => m (Map Name Type)
getTypedefs =
  liftModuleBuilderState $ gets types

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

globalUtf8StringPtr :: (HasCallStack, MonadNameSupply m, MonadModuleBuilder m, MonadIRBuilder m)
                    => T.Text -> Name -> m Operand
globalUtf8StringPtr txt name = do
  let utf8Bytes = BS.snoc (TE.encodeUtf8 txt) 0  -- 0-terminated UTF8 string
      llvmValues = map (Int 8 . toInteger) $ BS.unpack utf8Bytes
      arrayValue = Array i8 llvmValues
      constant = ConstantOperand arrayValue
      ty = typeOf constant
  -- This definition will end up before the function this is used in
  addr <- global name ty arrayValue
  let instr = GetElementPtr On addr [ ConstantOperand $ Int 32 0
                                    , ConstantOperand $ Int 32 0
                                    ]
  emitInstr (ptr i8) instr

-- NOTE: typedefs are only allowed for structs, even though clang also allows it
-- for primitive types. This is done to avoid weird inconsistencies with the LLVM JIT
-- (where this is not allowed).
typedef :: MonadModuleBuilder m => Name -> Flag Packed -> [Type] -> m Type
typedef name packed tys = do
  let ty = StructureType packed tys
  emitDefinition $ TypeDefinition name (Clear ty)
  addType name ty
  pure $ NamedTypeReference name

opaqueTypedef :: MonadModuleBuilder m => Name -> m Type
opaqueTypedef name = do
  emitDefinition $ TypeDefinition name Opaque
  pure $ NamedTypeReference name

extern :: MonadModuleBuilder m => Name -> [Type] -> Type -> m Operand
extern name argTys retTy = do
  let args = [(argTy, ParameterName "") | argTy <- argTys]
  fnAttrs <- getDefaultFunctionAttributes
  emitDefinition $ GlobalDefinition $ Function name retTy args fnAttrs []
  let fnTy = ptr $ FunctionType retTy argTys
  pure $ ConstantOperand $ GlobalRef fnTy name

-- NOTE: Only used internally, this creates an unassigned operand
mkOperand :: Monad m => Type -> ParameterName -> IRBuilderT m (Name, Operand)
mkOperand ty paramName = do
  name <- case paramName of
    NoParameterName -> fresh
    ParameterName name -> fresh `named` Name name
  pure (name, LocalRef ty name)

