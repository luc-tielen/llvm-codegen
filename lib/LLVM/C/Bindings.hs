module LLVM.C.Bindings
  ( Context
  , Module
  , TargetData
  , Type
  , llvmContextCreate
  , llvmContextDispose
  , llvmCreateModuleWithName
  , llvmDisposeModule
  , llvmGetTargetData
  , llvmSizeOfType
  , llvmVoidTypeInContext
  , llvmI1TypeInContext
  , llvmI8TypeInContext
  , llvmI16TypeInContext
  , llvmI32TypeInContext
  , llvmI64TypeInContext
  , llvmIntTypeInContext
  , llvmPointerTypeInContext
  , llvmStructTypeInContext
  , llvmNamedStructTypeInContext
  , llvmNamedStructSetBody
  , llvmArrayTypeInContext
  , llvmFunctionTypeInContext
  , llvmGetTypeByNameInContext
  ) where

import Foreign.C
import Foreign.Ptr


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

foreign import ccall unsafe "LLVMStructCreateNamed" llvmNamedStructTypeInContext
  :: Ptr Context -> CString -> IO (Ptr Type)

foreign import ccall unsafe "LLVMStructSetBody" llvmNamedStructSetBody
  :: Ptr Type -> Ptr (Ptr Type) -> CUInt -> CBool -> IO ()

foreign import ccall unsafe "LLVMArrayType" llvmArrayTypeInContext
  :: Ptr Type -> CUInt -> IO (Ptr Type)

foreign import ccall unsafe "LLVMFunctionType" llvmFunctionTypeInContext
  :: Ptr Type -> Ptr (Ptr Type) -> CUInt -> CBool -> IO (Ptr Type)

foreign import ccall unsafe "LLVMGetTypeByName2" llvmGetTypeByNameInContext
  :: Ptr Context -> CString -> IO (Ptr Type)
