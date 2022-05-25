{-# LANGUAGE RecursiveDo #-}

module LLVM.Codegen
  ( module LLVM.Codegen.IRBuilder
  , module LLVM.Codegen.ModuleBuilder
  , exampleModule -- TODO remove
  ) where

import LLVM.Codegen.NameSupply
import LLVM.Codegen.IRBuilder
import LLVM.Codegen.ModuleBuilder
import LLVM.Codegen.Type

import Control.Monad.IO.Class
import qualified LLVM.C.API as C


exampleModule :: IO Module
exampleModule = runModuleBuilderT $ do
  liftIO $ do
    ctx <- C.mkContext
    llvmMod <- C.mkModule ctx "test"
    td <- C.getTargetData llvmMod
    tys <- traverse (C.mkIntType ctx) [32, 64]
    structTy <- C.mkStructType ctx tys False
    size <- C.sizeOfType td structTy
    print (structTy, size)  -- Expect (ptr 0x..., 8)


  function (Name "do_add") [IntType 1, IntType 32] (IntType 8) $ \[x, y] -> mdo
    z <- add x y
    _ <- add x y

    _ <- block
    _ <- add y z
    ret y

-- $> Data.Text.IO.putStrLn =<< renderDoc <$> exampleModule

