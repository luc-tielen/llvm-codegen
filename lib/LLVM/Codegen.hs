{-# LANGUAGE RecursiveDo #-}

module LLVM.Codegen
  ( module LLVM.Codegen.IRBuilder
  , module LLVM.Codegen.ModuleBuilder
  , exampleModule -- TODO remove
  ) where

import LLVM.NameSupply
import LLVM.Codegen.IRBuilder
import LLVM.Codegen.ModuleBuilder
import LLVM.Codegen.Type


exampleModule :: Module
exampleModule = runModuleBuilder $ do
  function (Name "do_add") [IntType 1, IntType 32] (IntType 8) $ \[x, y] -> mdo
    z <- add x y
    _ <- add x y

    _ <- block
    _ <- add y z
    ret y

-- $> Data.Text.IO.putStrLn $ renderDoc exampleModule
