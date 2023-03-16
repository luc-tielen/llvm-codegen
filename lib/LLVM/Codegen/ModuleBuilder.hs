{-# LANGUAGE TypeFamilies, RankNTypes, UnboxedTuples, MultiParamTypeClasses, UndecidableInstances #-}

module LLVM.Codegen.ModuleBuilder
  ( ModuleBuilder
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
import qualified Control.Monad.State.Strict as StrictState
import qualified Control.Monad.State.Lazy as LazyState
import qualified Control.Monad.RWS.Lazy as LazyRWS
import qualified Control.Monad.RWS.Strict as StrictRWS
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Data.Map (Map)
import Data.IORef
import Data.String
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import LLVM.Codegen.IRBuilder.Monad
import LLVM.Codegen.Operand
import LLVM.Codegen.Type
import LLVM.Codegen.Name
import LLVM.Codegen.Flag
import LLVM.Codegen.NameSupply
import qualified LLVM.Codegen.ArrayList as L
import LLVM.Codegen.IR
import LLVM.Pretty


newtype Module
  = Module (V.Vector Definition)

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
  | Function !Name !Type ![(Type, ParameterName)] ![FunctionAttribute] !(V.Vector BasicBlock)

data Typedef
  = Opaque
  | Clear !Type
  deriving Show

data Definition
  = GlobalDefinition !Global
  | TypeDefinition !Name !Typedef

data ModuleBuilderState
  = ModuleBuilderState
  { definitions :: !(L.ArrayList Definition)
  , types :: !(Map Name Type)
  , defaultFunctionAttributes :: ![FunctionAttribute]
  }

newtype ModuleBuilder a
  = ModuleBuilder (ReaderT (IORef ModuleBuilderState) IO a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO)
  via ReaderT (IORef ModuleBuilderState) IO

class MonadIO m => MonadModuleBuilder m where
  getModuleBuilderStateRef :: m (IORef ModuleBuilderState)

  default getModuleBuilderStateRef
    :: (MonadTrans t, MonadModuleBuilder m1, m ~ t m1)
    => m (IORef ModuleBuilderState)
  getModuleBuilderStateRef =
    lift getModuleBuilderStateRef
  {-# INLINEABLE getModuleBuilderStateRef #-}

instance MonadModuleBuilder ModuleBuilder where
  getModuleBuilderStateRef = ModuleBuilder ask
  {-# INLINEABLE getModuleBuilderStateRef #-}

instance MonadModuleBuilder m => MonadModuleBuilder (IRBuilderT m)
instance MonadModuleBuilder m => MonadModuleBuilder (StrictState.StateT s m)
instance MonadModuleBuilder m => MonadModuleBuilder (LazyState.StateT s m)
instance (MonadModuleBuilder m, Monoid w) => MonadModuleBuilder (StrictRWS.RWST r w s m)
instance (MonadModuleBuilder m, Monoid w) => MonadModuleBuilder (LazyRWS.RWST r w s m)
instance MonadModuleBuilder m => MonadModuleBuilder (ReaderT r m)
instance (MonadModuleBuilder m, Monoid w) => MonadModuleBuilder (WriterT w m)
instance MonadModuleBuilder m => MonadModuleBuilder (ExceptT e m)

getModuleBuilderState :: MonadModuleBuilder m => m ModuleBuilderState
getModuleBuilderState = do
  ref <- getModuleBuilderStateRef
  liftIO $ readIORef ref
{-# INLINE getModuleBuilderState #-}

setModuleBuilderState :: MonadModuleBuilder m => (ModuleBuilderState -> ModuleBuilderState) -> m ()
setModuleBuilderState f = do
  ref <- getModuleBuilderStateRef
  liftIO $ modifyIORef' ref f
{-# INLINE setModuleBuilderState #-}

runModuleBuilder :: ModuleBuilder a -> IO Module
runModuleBuilder (ModuleBuilder m) =
  Module <$> do
    defs <- L.new 10  -- start with 10 pre-allocated definitions
    ref <- newIORef $ ModuleBuilderState defs mempty mempty
    _ <- runReaderT m ref
    L.toVector . definitions =<< readIORef ref
{-# INLINEABLE runModuleBuilder #-}

withFunctionAttributes
  :: MonadModuleBuilder m
  => ([FunctionAttribute] -> [FunctionAttribute])
  -> m a -> m a
withFunctionAttributes f m = do
  fnAttrs <- defaultFunctionAttributes <$> getModuleBuilderState
  setModuleBuilderState $ \s -> s { defaultFunctionAttributes = f fnAttrs }
  result <- m
  setModuleBuilderState $ \s -> s { defaultFunctionAttributes = fnAttrs }
  pure result
{-# INLINEABLE withFunctionAttributes #-}

resetFunctionAttributes :: MonadModuleBuilder m => m ()
resetFunctionAttributes = do
  setModuleBuilderState $ \s -> s { defaultFunctionAttributes = mempty }
{-# INLINEABLE resetFunctionAttributes #-}

getDefaultFunctionAttributes :: MonadModuleBuilder m => m [FunctionAttribute]
getDefaultFunctionAttributes = do
  defaultFunctionAttributes <$> getModuleBuilderState
{-# INLINEABLE getDefaultFunctionAttributes #-}

function :: (HasCallStack, MonadModuleBuilder m)
         => Name -> [(Type, ParameterName)] -> Type -> ([Operand] -> IRBuilderT m a) -> m Operand
function name args retTy fnBody = do
  fnAttrs <- getDefaultFunctionAttributes

  (names, instrs) <- runIRBuilderT $ do
    (names, operands) <- unzip <$> traverse (uncurry mkOperand) args
    resetFunctionAttributes  -- This is done to avoid functions emitted in the body that not automatically copy the same attributes
    _ <- fnBody operands
    pure names

  setModuleBuilderState $ \s -> s { defaultFunctionAttributes = fnAttrs }
  let args' = zipWith (\argName (ty, _) -> (ty, ParameterName $ unName argName)) names args
  emitDefinition $ GlobalDefinition $ Function name retTy args' fnAttrs instrs
  pure $ ConstantOperand $ GlobalRef (ptr (FunctionType retTy $ map fst args)) name
{-# INLINEABLE function #-}

emitDefinition :: MonadModuleBuilder m => Definition -> m ()
emitDefinition def = do
  defs <- definitions <$> getModuleBuilderState
  defs' <- liftIO $ L.append def defs
  setModuleBuilderState $ \s -> s { definitions = defs' }
{-# INLINEABLE emitDefinition #-}

getTypedefs :: MonadModuleBuilder m => m (Map Name Type)
getTypedefs =
  types <$> getModuleBuilderState
{-# INLINEABLE getTypedefs #-}

lookupType :: MonadModuleBuilder m => Name -> m (Maybe Type)
lookupType name = do
  Map.lookup name . types <$> getModuleBuilderState
{-# INLINEABLE lookupType #-}

addType :: MonadModuleBuilder m => Name -> Type -> m ()
addType name ty = do
  setModuleBuilderState $ \s -> s { types = Map.insert name ty (types s) }
{-# INLINEABLE addType #-}

global :: MonadModuleBuilder m => Name -> Type -> Constant -> m Operand
global name ty constant = do
  emitDefinition $ GlobalDefinition $ GlobalVariable name ty constant
  pure $ ConstantOperand $ GlobalRef (ptr ty) name
{-# INLINEABLE global #-}

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
  emitDefinition $ GlobalDefinition $ Function name retTy args fnAttrs mempty
  let fnTy = ptr $ FunctionType retTy argTys
  pure $ ConstantOperand $ GlobalRef fnTy name
{-# INLINEABLE extern #-}

-- NOTE: Only used internally, this creates an unassigned operand
mkOperand :: Monad m => Type -> ParameterName -> IRBuilderT m (Name, Operand)
mkOperand ty paramName = do
  name <- case paramName of
    NoParameterName -> fresh
    ParameterName name -> fresh `named` Name name
  pure (name, LocalRef ty name)
{-# INLINEABLE mkOperand #-}

renderModule :: Renderer Module
renderModule buf (Module defs) =
  sepBy "\n\n"# buf (V.toList defs) renderDefinition
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
        |># (if null attrs then "{\n"# else " {\n"#)) (V.toList body) renderBasicBlock |># "\n}"#
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
