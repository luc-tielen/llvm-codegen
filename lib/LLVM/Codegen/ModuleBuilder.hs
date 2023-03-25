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
  , withFunctionAttributes
  , renderModule
  ) where

import GHC.Stack
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
import Data.String
import qualified Data.DList as DList
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Data.Functor.Identity
import LLVM.Codegen.IRBuilder.Monad
import LLVM.Codegen.Operand
import LLVM.Codegen.Type
import LLVM.Codegen.Name
import LLVM.Codegen.Flag
import LLVM.Codegen.IR
import LLVM.Pretty


newtype Module
  = Module [Definition]

data ParameterName
  = ParameterName !T.Text
  | NoParameterName
  deriving Show

instance IsString ParameterName where
  fromString = ParameterName . fromString

data FunctionAttribute
  = WasmExportName !T.Text
  | AlwaysInline
  -- Add more as needed..
  deriving Show

data Global
  = GlobalVariable !Name !Type !Constant
  | Function !Name !Type ![(Type, ParameterName)] ![FunctionAttribute] ![BasicBlock]
  deriving Show

data Typedef
  = Opaque
  | Clear !Type
  deriving Show

data Definition
  = GlobalDefinition !Global
  | TypeDefinition !Name !Typedef
  deriving Show

data ModuleBuilderState
  = ModuleBuilderState
  { definitions :: !(DList Definition)
  , types :: !(Map Name Type)
  , defaultFunctionAttributes :: ![FunctionAttribute]
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
  {-# INLINEABLE lift #-}

instance MonadReader r m => MonadReader r (ModuleBuilderT m) where
  ask = lift ask
  {-# INLINEABLE ask #-}
  local = mapModuleBuilderT . local
  {-# INLINEABLE local #-}

mapModuleBuilderT :: (Functor m, Monad n) => (m a -> n a) -> ModuleBuilderT m a -> ModuleBuilderT n a
mapModuleBuilderT f (ModuleBuilderT inner) =
  ModuleBuilderT $ do
    s <- LazyState.get
    LazyState.mapStateT (g s) inner
  where
    g s = fmap (,s) . f . fmap fst
{-# INLINEABLE mapModuleBuilderT #-}

instance MonadState s m => MonadState s (ModuleBuilderT m) where
  state = lift . LazyState.state
  {-# INLINEABLE state #-}

instance MFunctor ModuleBuilderT where
  hoist nat = ModuleBuilderT . hoist nat . unModuleBuilderT
  {-# INLINEABLE hoist #-}

class Monad m => MonadModuleBuilder m where
  liftModuleBuilderState :: State ModuleBuilderState a -> m a

  default liftModuleBuilderState
    :: (MonadTrans t, MonadModuleBuilder m1, m ~ t m1)
    => State ModuleBuilderState a
    -> m a
  liftModuleBuilderState = lift . liftModuleBuilderState
  {-# INLINEABLE liftModuleBuilderState #-}

instance Monad m => MonadModuleBuilder (ModuleBuilderT m) where
  liftModuleBuilderState (StateT s) =
    ModuleBuilderT $ StateT $ pure . runIdentity . s
  {-# INLINEABLE liftModuleBuilderState #-}

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
{-# INLINEABLE runModuleBuilderT #-}

withFunctionAttributes
  :: MonadModuleBuilder m
  => ([FunctionAttribute] -> [FunctionAttribute])
  -> m a -> m a
withFunctionAttributes f m = do
  fnAttrs <- liftModuleBuilderState (gets defaultFunctionAttributes)
  liftModuleBuilderState $
    modify $ \s -> s { defaultFunctionAttributes = f fnAttrs }
  result <- m
  liftModuleBuilderState $
    modify $ \s -> s { defaultFunctionAttributes = fnAttrs }
  pure result
{-# INLINEABLE withFunctionAttributes #-}

resetFunctionAttributes :: MonadModuleBuilder m => m ()
resetFunctionAttributes =
  liftModuleBuilderState $
    modify $ \s -> s { defaultFunctionAttributes = mempty }
{-# INLINEABLE resetFunctionAttributes #-}

getDefaultFunctionAttributes :: MonadModuleBuilder m => m [FunctionAttribute]
getDefaultFunctionAttributes =
  liftModuleBuilderState $ gets defaultFunctionAttributes
{-# INLINEABLE getDefaultFunctionAttributes #-}

runModuleBuilder :: ModuleBuilder a -> Module
runModuleBuilder = runIdentity . runModuleBuilderT
{-# INLINEABLE runModuleBuilder #-}

function :: (HasCallStack, MonadModuleBuilder m)
         => Name -> [(Type, ParameterName)] -> Type -> ([Operand] -> IRBuilderT m a) -> m Operand
function name args retTy fnBody = do
  fnAttrs <- getDefaultFunctionAttributes

  (names, instrs) <- runIRBuilderT $ do
    (names, operands) <- unzip <$> traverse (uncurry mkOperand) args
    resetFunctionAttributes  -- This is done to avoid functions emitted in the body that not automatically copy the same attributes
    _ <- fnBody operands
    pure names

  liftModuleBuilderState $
    modify $ \s -> s { defaultFunctionAttributes = fnAttrs }
  let args' = zipWith (\argName (ty, _) -> (ty, ParameterName $ unName argName)) names args
  emitDefinition $ GlobalDefinition $ Function name retTy args' fnAttrs instrs
  pure $ ConstantOperand $ GlobalRef (ptr (FunctionType retTy $ map fst args)) name
{-# INLINEABLE function #-}

emitDefinition :: MonadModuleBuilder m => Definition -> m ()
emitDefinition def =
  liftModuleBuilderState $ modify $ \s -> s { definitions = DList.snoc (definitions s) def }
{-# INLINEABLE emitDefinition #-}

getTypedefs :: MonadModuleBuilder m => m (Map Name Type)
getTypedefs =
  liftModuleBuilderState $ gets types
{-# INLINEABLE getTypedefs #-}

lookupType :: MonadModuleBuilder m => Name -> m (Maybe Type)
lookupType name =
  liftModuleBuilderState $ gets (Map.lookup name . types)
{-# INLINEABLE lookupType #-}

addType :: MonadModuleBuilder m => Name -> Type -> m ()
addType name ty =
  liftModuleBuilderState $ modify $ \s -> s { types = Map.insert name ty (types s) }
{-# INLINEABLE addType #-}

global :: MonadModuleBuilder m => Name -> Type -> Constant -> m Operand
global name ty constant = do
  emitDefinition $ GlobalDefinition $ GlobalVariable name ty constant
  pure $ ConstantOperand $ GlobalRef (ptr ty) name
{-# INLINEABLE global #-}

globalUtf8StringPtr :: (HasCallStack, MonadModuleBuilder m, MonadIRBuilder m)
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
{-# INLINEABLE globalUtf8StringPtr #-}

-- NOTE: typedefs are only allowed for structs, even though clang also allows it
-- for primitive types. This is done to avoid weird inconsistencies with the LLVM JIT
-- (where this is not allowed).
typedef :: MonadModuleBuilder m => Name -> Flag Packed -> [Type] -> m Type
typedef name packed tys = do
  let ty = StructureType packed tys
  emitDefinition $ TypeDefinition name (Clear ty)
  addType name ty
  pure $ NamedTypeReference name
{-# INLINEABLE typedef #-}

opaqueTypedef :: MonadModuleBuilder m => Name -> m Type
opaqueTypedef name = do
  emitDefinition $ TypeDefinition name Opaque
  pure $ NamedTypeReference name
{-# INLINEABLE opaqueTypedef #-}

extern :: MonadModuleBuilder m => Name -> [Type] -> Type -> m Operand
extern name argTys retTy = do
  let args = [(argTy, ParameterName "") | argTy <- argTys]
  fnAttrs <- getDefaultFunctionAttributes
  emitDefinition $ GlobalDefinition $ Function name retTy args fnAttrs []
  let fnTy = ptr $ FunctionType retTy argTys
  pure $ ConstantOperand $ GlobalRef fnTy name
{-# INLINEABLE extern #-}

-- NOTE: Only used internally, this creates an unassigned operand
mkOperand :: Monad m => Type -> ParameterName -> IRBuilderT m (Name, Operand)
mkOperand ty paramName = do
  name <- case paramName of
    NoParameterName -> freshName Nothing
    ParameterName name -> freshName (Just name)
  pure (name, LocalRef ty name)
{-# INLINEABLE mkOperand #-}

renderModule :: Renderer Module
renderModule buf (Module defs) =
  sepBy "\n\n"# buf defs renderDefinition
{-# INLINEABLE renderModule #-}

renderDefinition :: Renderer Definition
renderDefinition buf = \case
  GlobalDefinition g ->
    renderGlobal buf g
  TypeDefinition name typeDef ->
    case typeDef of
      Opaque ->
        (buf |>. '%') `renderName` name |># " = type opaque"#
      Clear ty ->
        ((buf |>. '%') `renderName` name |># " = type "#) `renderType` ty
{-# INLINEABLE renderDefinition #-}

renderGlobal :: Renderer Global
renderGlobal buf = \case
  GlobalVariable name ty constant ->
    (((((buf |>. '@') `renderName` name) |># " = global "#) `renderType` ty) |>. ' ') `renderConstant` constant
  Function name retTy args attrs body
    | null body ->
      hsep (tupled ((((buf |># "declare external ccc "#) `renderType` retTy) |># " @"#) `renderName` name) argTys renderType
        |># (if null attrs then ""# else " "#)) attrs renderFunctionAttr
    | otherwise ->
      vsep (hsep (tupled ((((buf |># "define external ccc "#) `renderType` retTy) |># " @"#) `renderName` name) (zip [0..] args) renderArg |>. ' ') attrs renderFunctionAttr
        |># (if null attrs then "{\n"# else " {\n"#)) body renderBasicBlock |># "\n}"#
    where
      argTys = map fst args
      renderArg :: Renderer (Int, (Type, ParameterName))
      renderArg buf' (i, (argTy, nm)) =
        let localRef = case nm of
              NoParameterName ->
                LocalRef argTy $ Name $ T.pack $ show i
              ParameterName paramName ->
                LocalRef argTy $ Name paramName
         in ((buf' `renderType` argTy) |>. ' ') `renderOperand` localRef
{-# INLINEABLE renderGlobal #-}

renderFunctionAttr :: Renderer FunctionAttribute
renderFunctionAttr buf = \case
  AlwaysInline ->
    buf |># "alwaysinline"#
  WasmExportName name ->
    dquotes
      (dquotes buf (|># "wasm-export-name"#) |>. '=')
      (|> name)
{-# INLINEABLE renderFunctionAttr #-}
