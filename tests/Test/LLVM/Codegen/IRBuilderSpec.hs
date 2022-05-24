{-# LANGUAGE QuasiQuotes #-}

module Test.LLVM.Codegen.IRBuilderSpec
  ( module Test.LLVM.Codegen.IRBuilderSpec
  ) where

import Test.Hspec
import NeatInterpolation
import Data.Text (Text)
-- TODO use LLVM.Codegen import here?
import LLVM.Codegen.ModuleBuilder
import LLVM.Codegen.IRBuilder
import LLVM.Codegen.Type
import LLVM.Codegen.Operand
import LLVM.Pretty


checkIR :: ModuleBuilder a -> Text -> IO ()
checkIR llvmModule expectedOutput = do
  let ir = renderDoc $ runModuleBuilder llvmModule
  ir `shouldBe` expectedOutput

spec :: Spec
spec = describe "constructing LLVM IR" $ do
  -- Module level
  it "supports an empty module" pending

  it "supports global constants" $ do
    let ir = global "my_constant" i32 (Int 32 42)
    checkIR ir [text|
      @my_constant = global i32 42
      |]
    let ir2 = do
          _ <- global "my_constant" i32 (Int 32 42)
          global "my_constant2" i64 (Int 64 1000)
    checkIR ir2 [text|
      @my_constant = global i32 42
      @my_constant2 = global i64 1000
      |]

  it "supports type definitions" $ do
    let ir = do
          myType <- typedef "my_type" $ ArrayType 10 i16
          typedef "my_type2" $ StructureType False [myType, myType]
    checkIR ir [text|
      %my_type = [10 x i16]
      %my_type2 = {%my_type, %my_type}
      |]
    -- TODO check all types

  it "supports functions" pending

  it "renders definitions in order they are defined" pending

  -- IR level

  it "supports defining basic blocks" pending

  it "supports giving a basic block a user-defined name" pending

  it "supports giving instruction operands a user-defined name" pending

  it "avoids name collissions by appending a unique suffix" pending

  it "supports 'add' instruction" pending

  it "supports 'mul instruction" pending

  it "supports 'sub instruction" pending

  it "supports 'udiv' instruction" pending

  it "supports 'and' instruction" pending

  it "supports 'trunc' instruction" pending

  it "supports 'zext' instruction" pending

  it "supports 'ptrtoint' instruction" pending

  it "supports 'bitcast' instruction" pending

  it "supports 'icmp' instruction" pending

  it "supports 'alloca' instruction" pending

  it "supports 'gep' instruction" pending

  it "supports 'load' instruction" pending

  it "supports 'store' instruction" pending

  it "supports 'phi' instruction" pending

  it "supports 'call' instruction" pending

  it "supports 'ret' instruction" pending

  it "supports 'retVoid' instruction" pending

  it "supports 'br' instruction" pending

  it "supports 'condBr' instruction" pending

  it "supports 'switch' instruction" pending

  it "supports 'select' instruction" pending

  it "supports 'bit' for creating i1 values" pending

  it "supports 'int8' for creating i8 values" pending

  it "supports 'int16' for creating i16 values" pending

  it "supports 'int32' for creating i32 values" pending

  it "supports 'int64' for creating i64 values" pending

  it "supports 'intN' for creating iN values" pending

  it "renders instructions in order they are defined" pending
