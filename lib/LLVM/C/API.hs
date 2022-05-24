module LLVM.C.API
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
  , getTypeByName
  ) where

import Data.Word
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Control.Exception
import Data.Text (Text)
import qualified Data.Text as T
import LLVM.C.Bindings
import LLVM.NameSupply  -- TODO only import Name


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

mkArrayType :: Ptr Type -> Word32 -> IO (Ptr Type)
mkArrayType elemTy count =
  llvmArrayTypeInContext elemTy (CUInt count)

mkFunctionType :: Ptr Type -> [Ptr Type] -> IO (Ptr Type)
mkFunctionType retTy argTys =
  withArray argTys $ \argTyArray -> do
    let argCount = CUInt $ fromIntegral $ length argTys
        isVarArg = CBool 0
    llvmFunctionTypeInContext retTy argTyArray argCount isVarArg

getTypeByName :: ForeignPtr Context -> Name -> IO (Ptr Type)
getTypeByName ctx (Name name) =
  withForeignPtr ctx $ \c ->
    withCString (T.unpack name) $ \str ->
      llvmGetTypeByNameInContext c str

mkContext :: IO (ForeignPtr Context)
mkContext = mask_ $ do
  ctx <- llvmContextCreate
  newForeignPtr llvmContextDispose ctx

mkModule :: ForeignPtr Context -> Text -> IO (ForeignPtr Module)
mkModule ctx name =
  withCString (T.unpack name) $ \name' -> do
    withForeignPtr ctx $ \c -> mask_ $ do
      llvmModule <- llvmCreateModuleWithName name' c
      -- TODO next line causes segfault? is this because some other field needs to get set first? or auto-cleaned up in context?
      -- newForeignPtr llvmDisposeModule llvmModule
      -- TODO: no longer need foreignptr wrapper then?
      newForeignPtr_  llvmModule

getTargetData :: ForeignPtr Module -> IO (Ptr TargetData)
getTargetData llvmModule =
  withForeignPtr llvmModule llvmGetTargetData

sizeOfType :: Ptr TargetData -> Ptr Type -> IO Word64
sizeOfType td ty = do
  CSize byteSize <- llvmSizeOfType td ty
  pure byteSize
