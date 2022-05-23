module Test.LLVM.C.APISpec
  ( module Test.LLVM.C.APISpec
  ) where

import Test.Hspec
import qualified LLVM.C.API as C
import Foreign

spec :: Spec
spec = describe "LLVM C API" $ parallel $ do
  it "can create a LLVM context" $ do
    ctx <- C.mkContext
    withForeignPtr ctx $ \c ->
      c `shouldNotBe` nullPtr
