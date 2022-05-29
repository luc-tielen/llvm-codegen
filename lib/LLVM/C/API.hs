module LLVM.C.API
  ( Context
  , Module
  , TargetData
  , Type
  , mkContext
  , mkModule
  , getTargetData
  , sizeOfType

  , getTypeByName
  , mkVoidType
  , mkIntType
  , mkPointerType
  , mkArrayType
  , mkFunctionType
  , mkAnonStructType
  , mkOpaqueStructType
  , setNamedStructBody
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
import LLVM.Codegen.Name
import LLVM.Codegen.Flag
import qualified LLVM.Codegen.Type as LLVMType


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

mkAnonStructType :: ForeignPtr Context -> [Ptr Type] -> Flag LLVMType.Packed -> IO (Ptr Type)
mkAnonStructType ctx tys packed =
  withForeignPtr ctx $ \c ->
    withArray tys $ \tyArray -> do
      let count = CUInt $ fromIntegral $ length tys
          packed' = CBool (if packed == On then 1 else 0)
      llvmStructTypeInContext c tyArray count packed'

-- NOTE: can be used to forward declare a struct type
mkOpaqueStructType :: ForeignPtr Context -> Name -> IO (Ptr Type)
mkOpaqueStructType ctx name =
  withForeignPtr ctx $ \c ->
    withNameAsCString name $ \nm ->
      llvmNamedStructTypeInContext c nm

withNameAsCString :: Name -> (CString -> IO a) -> IO a
withNameAsCString name  =
  withCString (T.unpack $ unName name)

-- NOTE: call this on a Type returned by 'mkOpaqueStructType' to define the struct body of that type.
setNamedStructBody :: ForeignPtr Context -> Ptr Type -> [Ptr Type] -> Flag LLVMType.Packed -> IO ()
setNamedStructBody ctx structTy tys packed =
  withForeignPtr ctx $ \c ->
    withArray tys $ \tyArray -> do
      let count = CUInt $ fromIntegral $ length tys
          packed' = CBool (if packed == On then 1 else 0)
      llvmNamedStructSetBody c structTy tyArray count packed'

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
