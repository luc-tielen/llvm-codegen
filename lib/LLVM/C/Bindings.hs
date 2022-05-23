module LLVM.C.Bindings
  ( Context
  , Module
  , TargetData
  , Type
  , mkContext
  , mkModule
  , getTargetData
  , sizeOfType

  , mkVoidType
  , mkIntType
  , mkPointerType
  , mkStructType
  , mkArrayType
  , mkFunctionType
  ) where

import Data.Word
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Control.Exception
import Data.Text (Text)
import qualified Data.Text as T


-- TODO: split into multiple modules: bindings.hs, api.hs, ...
-- TODO: use ReaderT (ForeignPtr Context) IO a

data Context
data Module
data TargetData
data Type

foreign import ccall unsafe "LLVMContextCreate" llvmContextCreate
  :: IO (Ptr Context)

foreign import ccall unsafe "&LLVMContextDispose" llvmContextDispose
  :: FunPtr (Ptr Context -> IO ())

foreign import ccall unsafe "LLVMModuleCreateWithNameInContext" llvmCreateModuleWithName
  :: CString -> Ptr Context -> IO (Ptr Module)

foreign import ccall unsafe "&LLVMDisposeModule" llvmDisposeModule
  :: FunPtr (Ptr Module -> IO ())

foreign import ccall unsafe "LLVMGetModuleDataLayout" llvmGetTargetData
  :: Ptr Module -> IO (Ptr TargetData)

foreign import ccall unsafe "LLVMABISizeOfType" llvmSizeOfType
  :: Ptr TargetData -> Ptr Type -> IO CSize

foreign import ccall unsafe "LLVMVoidTypeInContext" llvmVoidTypeInContext
  :: Ptr Context -> IO (Ptr Type)

foreign import ccall unsafe "LLVMInt1TypeInContext" llvmI1TypeInContext
  :: Ptr Context -> IO (Ptr Type)

foreign import ccall unsafe "LLVMInt8TypeInContext" llvmI8TypeInContext
  :: Ptr Context -> IO (Ptr Type)

foreign import ccall unsafe "LLVMInt16TypeInContext" llvmI16TypeInContext
  :: Ptr Context -> IO (Ptr Type)

foreign import ccall unsafe "LLVMInt32TypeInContext" llvmI32TypeInContext
  :: Ptr Context -> IO (Ptr Type)

foreign import ccall unsafe "LLVMInt64TypeInContext" llvmI64TypeInContext
  :: Ptr Context -> IO (Ptr Type)

foreign import ccall unsafe "LLVMIntTypeInContext" llvmIntTypeInContext
  :: Ptr Context -> CUInt -> IO (Ptr Type)

foreign import ccall unsafe "LLVMPointerType" llvmPointerTypeInContext
  :: Ptr Type -> CUInt -> IO (Ptr Type)

foreign import ccall unsafe "LLVMStructTypeInContext" llvmStructTypeInContext
  :: Ptr Context -> Ptr (Ptr Type) -> CUInt -> CBool -> IO (Ptr Type)

foreign import ccall unsafe "LLVMArrayType" llvmArrayTypeInContext
  :: Ptr Type -> CUInt -> IO (Ptr Type)

foreign import ccall unsafe "LLVMFunctionType" llvmFunctionTypeInContext
  :: Ptr Type -> Ptr (Ptr Type) -> CUInt -> CBool -> IO (Ptr Type)


mkVoidType :: ForeignPtr Context -> IO (Ptr Type)
mkVoidType ctx = withForeignPtr ctx llvmVoidTypeInContext

mkIntType :: ForeignPtr Context -> Word32 -> IO (Ptr Type)
mkIntType ctx bits = withForeignPtr ctx $ \c ->
  case bits of
    1 -> llvmI1TypeInContext c
    8 -> llvmI8TypeInContext c
    16 -> llvmI16TypeInContext c
    32 -> llvmI32TypeInContext c
    64 -> llvmI64TypeInContext c
    _ -> llvmIntTypeInContext c (CUInt bits)

mkPointerType :: Ptr Type -> IO (Ptr Type)
mkPointerType pointeeTy =
  llvmPointerTypeInContext pointeeTy 0

mkStructType :: ForeignPtr Context -> [Ptr Type] -> Bool -> IO (Ptr Type)
mkStructType ctx tys packed =
  withForeignPtr ctx $ \c ->
    withArray tys $ \tyArray -> do
      let count = CUInt $ fromIntegral $ length tys
          packed' = CBool (if packed then 1 else 0)
      llvmStructTypeInContext c tyArray count packed'

mkFunctionType :: Ptr Type -> [Ptr Type] -> IO (Ptr Type)
mkFunctionType retTy argTys =
  withArray argTys $ \argTyArray -> do
    let argCount = CUInt $ fromIntegral $ length argTys
        isVarArg = CBool 0
    llvmFunctionTypeInContext retTy argTyArray argCount isVarArg

mkArrayType :: Ptr Type -> Word32 -> IO (Ptr Type)
mkArrayType elemTy count =
  llvmArrayTypeInContext elemTy (CUInt count)

mkContext :: IO (ForeignPtr Context)
mkContext = mask_ $ do
  ctx <- llvmContextCreate
  newForeignPtr llvmContextDispose ctx

mkModule :: ForeignPtr Context -> Text -> IO (ForeignPtr Module)
mkModule ctx name =
  withCString (T.unpack name) $ \name' -> do
    withForeignPtr ctx $ \c -> mask_ $ do
      llvmModule <- llvmCreateModuleWithName name' c
      newForeignPtr llvmDisposeModule llvmModule

getTargetData :: ForeignPtr Module -> IO (Ptr TargetData)
getTargetData llvmModule =
  withForeignPtr llvmModule llvmGetTargetData

sizeOfType :: Ptr TargetData -> Ptr Type -> IO Word64
sizeOfType td ty = do
  CSize byteSize <- llvmSizeOfType td ty
  pure byteSize
