module LLVM.C.Bindings
  ( Context
  , Module
  , TargetData
  , Type
  , mkContext
  , mkModule
  , getTargetData
  , sizeOfType
  ) where

import Data.Word
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Exception
import Data.Text (Text)
import qualified Data.Text as T

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


-- TODO: bindings to create following types: ints, pointers, void, struct, array (, function?)

-- TODO: split into multiple modules: bindings.hs, api.hs, ...
-- TODO: use ReaderT (ForeignPtr Context) IO a
