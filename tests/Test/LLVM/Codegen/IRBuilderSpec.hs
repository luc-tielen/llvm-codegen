{-# LANGUAGE QuasiQuotes, RecursiveDo #-}

module Test.LLVM.Codegen.IRBuilderSpec
  ( module Test.LLVM.Codegen.IRBuilderSpec
  ) where

import Prelude hiding (and)
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
          _ <- typedef "i1_synonym" i1
          _ <- typedef "i32_synonym" i32
          _ <- typedef "pointer_to_i32" (ptr i32)
          _ <- typedef "ptr_to_ptr" (ptr (ptr i32))
          _ <- typedef "void_type" void
          myType <- typedef "my_type" $ ArrayType 10 i16
          _ <- typedef "my_type2" $ StructureType False [myType, myType]
          typedef "my_type2_packed" $ StructureType True [myType, myType]
    checkIR ir [text|
      %i1_synonym = type i1
      %i32_synonym = type i32
      %pointer_to_i32 = type i32*
      %ptr_to_ptr = type i32**
      %void_type = type void
      %my_type = type [10 x i16]
      %my_type2 = type {%my_type, %my_type}
      %my_type2_packed = type <{%my_type, %my_type}>
      |]

  it "supports functions" $ do
    let ir = do
          function "do_add" [i32, i32] i32 $ \[a, b] -> do
            c <- add a b
            ret c
    checkIR ir [text|
      define external ccc i32 @do_add(i32 %0, i32 %1) {
      start:
        %2 = add i32 %0, %1
        ret i32 %2
      }
      |]

  it "renders functions in order they are defined" $ do
    let ir = do
          _ <- function "do_add" [i32, i32] i32 $ \[a, b] -> do
            c <- add a b
            ret c
          function "do_add2" [i32, i32] i32 $ \[a, b] -> do
            c <- add a b
            ret c
    checkIR ir [text|
      define external ccc i32 @do_add(i32 %0, i32 %1) {
      start:
        %2 = add i32 %0, %1
        ret i32 %2
      }
      define external ccc i32 @do_add2(i32 %0, i32 %1) {
      start:
        %2 = add i32 %0, %1
        ret i32 %2
      }
      |]

  -- IR level

  it "supports defining basic blocks" $ do
    let ir = do
          function "do_add" [i32, i32] i32 $ \[a, b] -> do
            _ <- block
            c <- add a b
            ret c
    checkIR ir [text|
      define external ccc i32 @do_add(i32 %0, i32 %1) {
      block_0:
        %2 = add i32 %0, %1
        ret i32 %2
      }
      |]

  it "supports giving a basic block a user-defined name" $ do
    let ir = do
          function "do_add" [i32, i32] i32 $ \[a, b] -> do
            _ <- block `named` "begin"
            c <- add a b
            ret c
    checkIR ir [text|
      define external ccc i32 @do_add(i32 %0, i32 %1) {
      begin_0:
        %2 = add i32 %0, %1
        ret i32 %2
      }
      |]

  it "automatically terminates previous basic block when starting new block" $ do
    let ir = do
          function "do_add" [i32, i32] i32 $ \[a, b] -> mdo
            c <- add a b
            -- NOTE: invalid IR
            _ <- block `named` "next"
            ret c
    checkIR ir [text|
      define external ccc i32 @do_add(i32 %0, i32 %1) {
      start:
        %2 = add i32 %0, %1
        ret void
      next_0:
        ret i32 %2
      }
      |]

  it "supports giving instruction operands a user-defined name" $ do
    let ir = do
          function "do_add" [i32, i32] i32 $ \[a, b] -> mdo
            c <- add a b `named` "c"
            d' <- flip named "d" $ do
              d <- add b c
              add d (int32 42)
            ret d'
    checkIR ir [text|
      define external ccc i32 @do_add(i32 %0, i32 %1) {
      start:
        %c_0 = add i32 %0, %1
        %d_0 = add i32 %1, %c_0
        %d_1 = add i32 %d_0, 42
        ret i32 %d_1
      }
      |]

  it "avoids name collissions by appending a unique suffix" $ do
    let ir = do
          function "do_add" [i32, i32] i32 $ \[a, b] -> mdo
            c <- flip named "c" $ do
              c0 <- add a b
              add c0 c0
            ret c
    checkIR ir [text|
      define external ccc i32 @do_add(i32 %0, i32 %1) {
      start:
        %c_0 = add i32 %0, %1
        %c_1 = add i32 %c_0, %c_0
        ret i32 %c_1
      }
      |]

  it "supports 'add' instruction" $ do
    let ir = do
          function "do_add" [i8, i8] i8 $ \[a, b] -> do
            c <- add a b
            ret c
    checkIR ir [text|
      define external ccc i8 @do_add(i8 %0, i8 %1) {
      start:
        %2 = add i8 %0, %1
        ret i8 %2
      }
      |]

  it "supports 'mul' instruction" $ do
    let ir = do
          function "func" [i8, i8] i8 $ \[a, b] -> do
            c <- mul a b
            ret c
    checkIR ir [text|
      define external ccc i8 @func(i8 %0, i8 %1) {
      start:
        %2 = mul i8 %0, %1
        ret i8 %2
      }
      |]

  it "supports 'sub' instruction" $ do
    let ir = do
          function "func" [i8, i8] i8 $ \[a, b] -> do
            c <- sub a b
            ret c
    checkIR ir [text|
      define external ccc i8 @func(i8 %0, i8 %1) {
      start:
        %2 = sub i8 %0, %1
        ret i8 %2
      }
      |]

  it "supports 'udiv' instruction" $ do
    let ir = do
          function "func" [i8, i8] i8 $ \[a, b] -> do
            c <- udiv a b
            ret c
    checkIR ir [text|
      define external ccc i8 @func(i8 %0, i8 %1) {
      start:
        %2 = udiv i8 %0, %1
        ret i8 %2
      }
      |]

  it "supports 'and' instruction" $ do
    let ir = do
          function "func" [i1, i1] i1 $ \[a, b] -> do
            c <- and a b
            ret c
    checkIR ir [text|
      define external ccc i1 @func(i1 %0, i1 %1) {
      start:
        %2 = and i1 %0, %1
        ret i1 %2
      }
      |]

  it "supports 'trunc' instruction" $ do
    let ir = do
          function "func" [i64] i32 $ \[a] -> do
            b <- trunc a i32
            ret b
    checkIR ir [text|
      define external ccc i32 @func(i64 %0) {
      start:
        %1 = trunc i64 %0 to i32
        ret i32 %1
      }
      |]

  it "supports 'zext' instruction" $ do
    let ir = do
          function "func" [i32] i64 $ \[a] -> do
            b <- zext a i64
            ret b
    checkIR ir [text|
      define external ccc i64 @func(i32 %0) {
      start:
        %1 = zext i32 %0 to i64
        ret i64 %1
      }
      |]

  it "supports 'ptrtoint' instruction" pending

  it "supports 'bitcast' instruction" pending

  -- TODO all comparisons
  it "supports 'icmp' instruction" pending

  it "supports 'alloca' instruction" pending

  it "supports 'gep' instruction" pending

  it "supports 'load' instruction" pending

  it "supports 'store' instruction" pending

  it "supports 'phi' instruction" pending

  it "supports 'call' instruction" pending

  it "supports 'ret' instruction" $ do
    let ir = do
          function "func" [i1] i1 $ \[a] -> do
            ret a
    checkIR ir [text|
      define external ccc i1 @func(i1 %0) {
      start:
        ret i1 %0
      }
      |]

  it "supports 'retVoid' instruction" $ do
    let ir = do
          function "func" [] void $ \[] -> do
            retVoid
    checkIR ir [text|
      define external ccc void @func() {
      start:
        ret void
      }
      |]

  it "only uses last terminator instruction" $ do
    let ir = do
          function "func" [] i1 $ \[] -> do
            ret (bit False)
            ret (bit True)
    checkIR ir [text|
      define external ccc i1 @func() {
      start:
        ret i1 1
      }
      |]

  it "supports 'br' instruction" $ do
    let ir = do
          function "func" [i1] i1 $ \[a] -> mdo
            br block2

            block1 <- block
            ret a

            block2 <- block
            br block1
    checkIR ir [text|
      define external ccc i1 @func(i1 %0) {
      start:
        br label %block_1
      block_0:
        ret i1 %0
      block_1:
        br label %block_0
      }
      |]

  it "supports 'condBr' instruction" $ do
    let ir = do
          function "func" [i1] i1 $ \[a] -> mdo
            condBr a block1 block2

            block1 <- block
            ret a

            block2 <- block
            condBr a block1 block3

            block3 <- block
            condBr a block1 block2
    checkIR ir [text|
      define external ccc i1 @func(i1 %0) {
      start:
        br i1 %0, label %block_0, label %block_1
      block_0:
        ret i1 %0
      block_1:
        br i1 %0, label %block_0, label %block_2
      block_2:
        br i1 %0, label %block_0, label %block_1
      }
      |]

  it "supports 'switch' instruction" pending

  it "supports 'select' instruction" $ do
    let ir = do
          function "not" [i1] i1 $ \[a] -> do
            b <- select a (bit False) (bit True)
            ret b
    checkIR ir [text|
      define external ccc i1 @not(i1 %0) {
      start:
        %1 = select i1 %0, i1 0, i1 1
        ret i1 %1
      }
      |]

  it "supports 'bit' for creating i1 values" $ do
    let ir = do
          function "func" [] i1 $ \[] -> do
            ret (bit True)
    checkIR ir [text|
      define external ccc i1 @func() {
      start:
        ret i1 1
      }
      |]
    let ir2 = do
          function "func" [] i1 $ \[] -> do
            ret (bit False)
    checkIR ir2 [text|
      define external ccc i1 @func() {
      start:
        ret i1 0
      }
      |]

  it "supports 'int8' for creating i8 values" $ do
    let ir = do
          function "func" [] i8 $ \[] -> do
            ret (int8 15)
    checkIR ir [text|
      define external ccc i8 @func() {
      start:
        ret i8 15
      }
      |]

  it "supports 'int16' for creating i16 values" $ do
    let ir = do
          function "func" [] i16 $ \[] -> do
            ret (int16 30)
    checkIR ir [text|
      define external ccc i16 @func() {
      start:
        ret i16 30
      }
      |]

  it "supports 'int32' for creating i32 values" $ do
    let ir = do
          function "func" [] i32 $ \[] -> do
            ret (int32 60)
    checkIR ir [text|
      define external ccc i32 @func() {
      start:
        ret i32 60
      }
      |]

  it "supports 'int64' for creating i64 values" $ do
    let ir = do
          function "func" [] i64 $ \[] -> do
            ret (int64 120)
    checkIR ir [text|
      define external ccc i64 @func() {
      start:
        ret i64 120
      }
      |]

  it "supports 'intN' for creating iN values" $ do
    let ir = do
          function "func" [] (IntType 42) $ \[] -> do
            ret (intN 42 1000)
    checkIR ir [text|
      define external ccc i42 @func() {
      start:
        ret i42 1000
      }
      |]

