module Test.LLVM.C.APISpec
  ( module Test.LLVM.C.APISpec
  ) where

import Test.Hspec
import Foreign hiding (void)
import qualified LLVM.C.API as C
import LLVM.Codegen.Type
import LLVM.Codegen.Name
import LLVM.Codegen.Flag

-- NOTE: if it can't find libffi, you're linking against wrong libLLVM!
-- Be sure to update Setup.hs LLVM version as well to be in sync!

mkType :: ForeignPtr C.Context -> Type -> IO (Ptr C.Type)
mkType ctx = \case
  VoidType ->
    C.mkVoidType ctx
  IntType bits ->
    C.mkIntType ctx bits
  PointerType ty ->
    C.mkPointerType =<< mkType ctx ty
  StructureType packed tys -> do
    tys' <- traverse (mkType ctx) tys
    C.mkAnonStructType ctx tys' packed
  ArrayType count ty -> do
    ty' <- mkType ctx ty
    C.mkArrayType ty' count
  FunctionType retTy argTys -> do
    retTy' <- mkType ctx retTy
    argTys' <- traverse (mkType ctx) argTys
    C.mkFunctionType retTy' argTys'
  NamedTypeReference name ->
    C.getTypeByName ctx name


-- TODO: do more than checking against nullptr in first tests

spec :: Spec
spec = describe "LLVM C API" $ parallel $ do
  it "can create a LLVM context" $ do
    ctx <- C.mkContext
    withForeignPtr ctx $ \c ->
      c `shouldNotBe` nullPtr

  it "can create an empty LLVM module" $ do
    ctx <- C.mkContext
    llvmMod <- C.mkModule ctx "test"
    withForeignPtr llvmMod $ \llvmModule ->
      llvmModule `shouldNotBe` nullPtr

  it "can extract the target data from a LLVM module" $ do
    ctx <- C.mkContext
    llvmMod <- C.mkModule ctx "test"
    td <- C.getTargetData llvmMod
    td `shouldNotBe` nullPtr

  let assertTypeSizes :: ((Type -> Word64 -> IO ()) -> IO ()) -> IO ()
      assertTypeSizes f = do
        ctx <- C.mkContext
        llvmMod <- C.mkModule ctx "test"
        td <- C.getTargetData llvmMod
        f $ \ty expectedSize -> do
          ty' <- mkType ctx ty
          actualSize <- C.sizeOfType td ty'
          actualSize `shouldBe` expectedSize

  it "can compute the size of an integer type" $ do
    assertTypeSizes $ \assert -> do
      assert i1 1
      assert i8 1
      assert i16 2
      assert i32 4
      assert i64 8

  -- NOTE: not allowed for "void" => has no size, causes SIGILL (due to missing return in a function in libLLVM / assert triggered)

  it "can compute the size of a pointer type" $ do
    assertTypeSizes $ \assert -> do
      assert (ptr i1) 8
      assert (ptr i32) 8
      assert (ptr i64) 8

  it "can compute the size of a struct type" $ do
    assertTypeSizes $ \assert -> do
      assert (StructureType Off [i8]) 1
      assert (StructureType Off [i8, i8]) 2
      assert (StructureType Off [i8, i16]) 4  -- padding!
      assert (StructureType On [i8, i16]) 3
      assert (StructureType On [i8, StructureType Off [i8]]) 2
      assert (StructureType Off [ArrayType 5 i32, i1]) 24  -- padding!
      assert (StructureType On [ArrayType 5 i32, i1]) 21
      assert (StructureType On [i32, i64]) 12  -- no padding, 4-byte alignment for i64?

  it "can compute the size of an array type" $ do
    assertTypeSizes $ \assert -> do
      assert (ArrayType 1 i1) 1
      assert (ArrayType 10 i1) 10
      assert (ArrayType 10 i32) 40
      assert (ArrayType 5 (StructureType Off [i32, i64])) (5 * 12)

  -- NOTE: not allowed for function type => has no size, triggers undefined behavior.

  it "returns null for an unknown named type reference" $ do
    let ty = NamedTypeReference $ Name "unknown"
    ctx <- C.mkContext
    ty' <- mkType ctx ty
    ty' `shouldBe` nullPtr

  -- TODO test for known named type ref, but need to first add to module
