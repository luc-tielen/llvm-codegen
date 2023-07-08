{-# LANGUAGE QuasiQuotes, RecursiveDo #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.LLVM.Codegen.IRBuilderSpec
  ( module Test.LLVM.Codegen.IRBuilderSpec
  ) where

import Prelude hiding (and, or, EQ)
import qualified Data.Text as T
import Data.Foldable hiding (and, or)
import Test.Hspec
import NeatInterpolation
import Data.Text (Text)
import LLVM.Codegen


checkIR :: ModuleBuilder a -> Text -> IO ()
checkIR llvmModule expectedOutput = do
  let ir = ppllvm $ runModuleBuilder llvmModule
  ir `shouldBe` expectedOutput

spec :: Spec
spec = describe "constructing LLVM IR" $ do
  -- Module level

  it "supports an empty module" $ do
    let ir = pure ()
    checkIR ir ""

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

  it "supports creating and using global utf8 string constants" $ do
    let ir = do
          function "utf8_string_usage" [] i8 $ \[] -> do
            str <- globalUtf8StringPtr "string_contents" "my_string"
            char <- load str 0
            ret char
    checkIR ir [text|
      @my_string = global [16 x i8] [i8 115, i8 116, i8 114, i8 105, i8 110, i8 103, i8 95, i8 99, i8 111, i8 110, i8 116, i8 101, i8 110, i8 116, i8 115, i8 0]

      define external ccc i8 @utf8_string_usage() {
      start:
        %0 = getelementptr inbounds [16 x i8], [16 x i8]* @my_string, i32 0, i32 0
        %1 = load i8, i8* %0
        ret i8 %1
      }
      |]

  it "supports type definitions" $ do
    let ir = mdo
          let myType = ArrayType 10 i16
          _ <- typedef "my_type2" Off [myType, myType]
          _ <- typedef "my_type2_packed" On [myType, myType]
          s <- typedef "recursive" Off [ptr s]
          _ <- opaqueTypedef "my_opaque_type"
          pure ()
    checkIR ir [text|
      %my_type2 = type {[10 x i16], [10 x i16]}

      %my_type2_packed = type <{[10 x i16], [10 x i16]}>

      %recursive = type {%recursive*}

      %my_opaque_type = type opaque
      |]

  it "supports external definitions" $ do
    let ir = do
          _ <- extern "symbol1" [i32, i64] (ptr i8)
          extern "symbol2" [] (ptr i8)
    checkIR ir [text|
      declare external ccc i8* @symbol1(i32, i64)

      declare external ccc i8* @symbol2()
      |]

  it "supports functions" $ do
    let ir = do
          function "do_add" [(i32, "a"), (i32, "b")] i32 $ \[a, b] -> do
            c <- add a b
            ret c
    checkIR ir [text|
      define external ccc i32 @do_add(i32 %a_0, i32 %b_0) {
      start:
        %0 = add i32 %a_0, %b_0
        ret i32 %0
      }
      |]

  it "renders functions in order they are defined" $ do
    let ir = do
          _ <- function "do_add" [(i32, "a"), (i32, "b")] i32 $ \[a, b] -> do
            c <- add a b
            ret c
          function "do_add2" [(i32, "a"), (i32, "b")] i32 $ \[a, b] -> do
            c <- add a b
            ret c
    checkIR ir [text|
      define external ccc i32 @do_add(i32 %a_0, i32 %b_0) {
      start:
        %0 = add i32 %a_0, %b_0
        ret i32 %0
      }

      define external ccc i32 @do_add2(i32 %a_0, i32 %b_0) {
      start:
        %0 = add i32 %a_0, %b_0
        ret i32 %0
      }
      |]

  -- IR level

  it "supports defining basic blocks" $ do
    let ir = do
          function "do_add" [(i32, "a"), (i32, "b")] i32 $ \[a, b] -> do
            _ <- block
            c <- add a b
            ret c
    checkIR ir [text|
      define external ccc i32 @do_add(i32 %a_0, i32 %b_0) {
      start:
        br label %block_0
      block_0:
        %0 = add i32 %a_0, %b_0
        ret i32 %0
      }
      |]

  it "supports giving a basic block a user-defined name" $ do
    let ir = do
          function "do_add" [(i32, "a"), (i32, "b")] i32 $ \[a, b] -> do
            c <- add a b
            ret c
    checkIR ir [text|
      define external ccc i32 @do_add(i32 %a_0, i32 %b_0) {
      start:
        %0 = add i32 %a_0, %b_0
        ret i32 %0
      }
      |]

  it "supports giving function parameters a user-defined name" $ do
    let ir = do
          function "do_add" [(i32, "arg0"), (i32, "arg1")] i32 $ \[a, b] -> do
            c <- add a b
            ret c
    checkIR ir [text|
      define external ccc i32 @do_add(i32 %arg0_0, i32 %arg1_0) {
      start:
        %0 = add i32 %arg0_0, %arg1_0
        ret i32 %0
      }
      |]

  it "supports automatic naming of function parameters" $ do
    let ir = do
          function "do_add" [(i32, NoParameterName), (i32, NoParameterName)] i32 $ \[a, b] -> do
            c <- add a b
            ret c
    checkIR ir [text|
      define external ccc i32 @do_add(i32 %0, i32 %1) {
      start:
        %2 = add i32 %0, %1
        ret i32 %2
      }
      |]

  it "automatically terminates previous basic block when starting new block" $ do
    let ir = do
          function "do_add" [(i32, "a"), (i32, "b")] i32 $ \[a, b] -> mdo
            c <- add a b
            -- NOTE: invalid IR
            _ <- blockNamed "next"
            ret c
    checkIR ir [text|
      define external ccc i32 @do_add(i32 %a_0, i32 %b_0) {
      start:
        %0 = add i32 %a_0, %b_0
        ret void
      next_0:
        ret i32 %0
      }
      |]

  it "avoids name collisions by appending a unique suffix" $ do
    let ir = do
          function "do_add" [(i32, "a"), (i32, "b")] i32 $ \[a, b] -> mdo
            _ <- blockNamed "blk"
            c <- add a b
            _ <- add c c
            br blk2
            blk2 <- blockNamed "blk"
            ret c
    checkIR ir [text|
      define external ccc i32 @do_add(i32 %a_0, i32 %b_0) {
      start:
        br label %blk_0
      blk_0:
        %0 = add i32 %a_0, %b_0
        %1 = add i32 %0, %0
        br label %blk_1
      blk_1:
        ret i32 %0
      }
      |]

  it "shifts allocas to start of the entry basic block" $ do
    let ir = do
          function "func" [(i32, "a")] i32 $ \[a] -> mdo
            _ <- alloca i32 Nothing 0
            b <- add a a
            br blk
            blk <- blockNamed "blk"
            _ <- alloca i64 Nothing 0
            c <- add b b
            ret c
    checkIR ir [text|
      define external ccc i32 @func(i32 %a_0) {
      start:
        %stack.ptr_0 = alloca i32
        %stack.ptr_1 = alloca i64
        %0 = add i32 %a_0, %a_0
        br label %blk_0
      blk_0:
        %1 = add i32 %0, %0
        ret i32 %1
      }
      |]

  it "supports 'add' instruction" $ do
    let ir = do
          function "do_add" [(i8, "a"), (i8, "b")] i8 $ \[a, b] -> do
            c <- add a b
            ret c
    checkIR ir [text|
      define external ccc i8 @do_add(i8 %a_0, i8 %b_0) {
      start:
        %0 = add i8 %a_0, %b_0
        ret i8 %0
      }
      |]

  it "supports 'mul' instruction" $ do
    let ir = do
          function "func" [(i8, "a"), (i8, "b")] i8 $ \[a, b] -> do
            c <- mul a b
            ret c
    checkIR ir [text|
      define external ccc i8 @func(i8 %a_0, i8 %b_0) {
      start:
        %0 = mul i8 %a_0, %b_0
        ret i8 %0
      }
      |]

  it "supports 'sub' instruction" $ do
    let ir = do
          function "func" [(i8, "a"), (i8, "b")] i8 $ \[a, b] -> do
            c <- sub a b
            ret c
    checkIR ir [text|
      define external ccc i8 @func(i8 %a_0, i8 %b_0) {
      start:
        %0 = sub i8 %a_0, %b_0
        ret i8 %0
      }
      |]

  it "supports 'udiv' instruction" $ do
    let ir = do
          function "func" [(i8, "a"), (i8, "b")] i8 $ \[a, b] -> do
            c <- udiv a b
            ret c
    checkIR ir [text|
      define external ccc i8 @func(i8 %a_0, i8 %b_0) {
      start:
        %0 = udiv i8 %a_0, %b_0
        ret i8 %0
      }
      |]

  it "supports 'and' instruction" $ do
    let ir = do
          function "func" [(i1, "a"), (i1, "b")] i1 $ \[a, b] -> do
            c <- and a b
            ret c
    checkIR ir [text|
      define external ccc i1 @func(i1 %a_0, i1 %b_0) {
      start:
        %0 = and i1 %a_0, %b_0
        ret i1 %0
      }
      |]

  it "supports 'or' instruction" $ do
    let ir = do
          function "func" [(i1, "a"), (i1, "b")] i1 $ \[a, b] -> do
            c <- or a b
            ret c
    checkIR ir [text|
      define external ccc i1 @func(i1 %a_0, i1 %b_0) {
      start:
        %0 = or i1 %a_0, %b_0
        ret i1 %0
      }
      |]

  it "supports 'trunc' instruction" $ do
    let ir = do
          function "func" [(i64, "a")] i32 $ \[a] -> do
            b <- trunc a i32
            ret b
    checkIR ir [text|
      define external ccc i32 @func(i64 %a_0) {
      start:
        %0 = trunc i64 %a_0 to i32
        ret i32 %0
      }
      |]

  it "supports 'zext' instruction" $ do
    let ir = do
          function "func" [(i32, "a")] i64 $ \[a] -> do
            b <- zext a i64
            ret b
    checkIR ir [text|
      define external ccc i64 @func(i32 %a_0) {
      start:
        %0 = zext i32 %a_0 to i64
        ret i64 %0
      }
      |]

  it "supports 'ptrtoint' instruction" $ do
    let ir = do
          function "func" [(ptr i32, "ptr_a")] i64 $ \[a] -> do
            b <- ptrtoint a i64
            ret b
    checkIR ir [text|
      define external ccc i64 @func(i32* %ptr_a_0) {
      start:
        %0 = ptrtoint i32* %ptr_a_0 to i64
        ret i64 %0
      }
      |]

  it "supports 'bitcast' instruction" $ do
    let ir = do
          function "func" [(ptr i32, "ptr_a")] (ptr i64) $ \[a] -> do
            b <- a `bitcast` ptr i64
            ret b
    checkIR ir [text|
      define external ccc i64* @func(i32* %ptr_a_0) {
      start:
        %0 = bitcast i32* %ptr_a_0 to i64*
        ret i64* %0
      }
      |]

  it "supports 'icmp' instruction" $ do
    let scenarios =
          [ (EQ, "eq")
          , (NE, "ne")
          , (ULE, "ule")
          , (UGT, "ugt")
          , (UGE, "uge")
          , (UGT, "ugt")
          , (SLE, "sle")
          , (SLT, "slt")
          , (SGE, "sge")
          , (SGT, "sgt")
          ]
    for_ scenarios $ \(cmp, cmpText) -> do
      let ir = do
            function "func" [(i32, "a"), (i32, "b")] i1 $ \[a, b] -> do
              c <- icmp cmp a b
              ret c
      checkIR ir [text|
        define external ccc i1 @func(i32 %a_0, i32 %b_0) {
        start:
          %0 = icmp $cmpText i32 %a_0, %b_0
          ret i1 %0
        }
        |]

  it "supports 'alloca' instruction" $ do
    let ir = do
          function "func" [(i32, "a")] i32 $ \[a] -> do
            _ <- alloca i64 Nothing 0
            _ <- alloca i1 (Just $ int32 8) 0
            _ <- alloca i1 Nothing 8
            ret a
    checkIR ir [text|
      define external ccc i32 @func(i32 %a_0) {
      start:
        %stack.ptr_0 = alloca i64
        %stack.ptr_1 = alloca i1, i32 8
        %stack.ptr_2 = alloca i1, align 8
        ret i32 %a_0
      }
      |]

  it "supports 'gep' instruction on pointers" $ do
    let ir = do
          function "func" [(ptr i64, "a"), (ptr (ptr (ptr i64)), "b")] (ptr i64) $ \[a, b] -> do
            c <- gep a [int32 1]
            _ <- gep b [int32 1, int32 2, int32 3]
            ret c
    checkIR ir [text|
      define external ccc i64* @func(i64* %a_0, i64*** %b_0) {
      start:
        %0 = getelementptr i64, i64* %a_0, i32 1
        %1 = getelementptr i64**, i64*** %b_0, i32 1, i32 2, i32 3
        ret i64* %0
      }
      |]

  it "supports 'gep' instruction on structs" $ do
    let ir = do
          struct1 <- typedef "my_struct" Off [i32, i64]
          struct2 <- typedef "my_struct2" Off [struct1, i1]

          function "func" [(ptr struct2, "a")] (ptr i64) $ \[a] -> do
            c <- gep a [int32 0, int32 0, int32 1]
            _ <- gep a [int32 0, int32 1]
            ret c
    checkIR ir [text|
      %my_struct = type {i32, i64}

      %my_struct2 = type {%my_struct, i1}

      define external ccc i64* @func(%my_struct2* %a_0) {
      start:
        %0 = getelementptr %my_struct2, %my_struct2* %a_0, i32 0, i32 0, i32 1
        %1 = getelementptr %my_struct2, %my_struct2* %a_0, i32 0, i32 1
        ret i64* %0
      }
      |]

  it "supports 'gep' instruction on arrays" $ do
    let ir = do
          let array = ArrayType 10 i32

          function "func" [(ptr array, "a")] (ptr i32) $ \[a] -> do
            c <- gep a [int32 0, int32 5]
            ret c
    checkIR ir [text|
      define external ccc i32* @func([10 x i32]* %a_0) {
      start:
        %0 = getelementptr [10 x i32], [10 x i32]* %a_0, i32 0, i32 5
        ret i32* %0
      }
      |]

  it "supports 'load' instruction" $ do
    let ir = do
          function "func" [(ptr i64, "a")] i64 $ \[a] -> do
            b <- load a 0
            _ <- load a 8
            ret b
    checkIR ir [text|
      define external ccc i64 @func(i64* %a_0) {
      start:
        %0 = load i64, i64* %a_0
        %1 = load i64, i64* %a_0, align 8
        ret i64 %0
      }
      |]

  it "supports 'store' instruction" $ do
    let ir = do
          function "func" [(ptr i64, "a")] void $ \[a] -> do
            store a 0 (int64 10)
            store a 8 (int64 10)
            retVoid
    checkIR ir [text|
      define external ccc void @func(i64* %a_0) {
      start:
        store i64 10, i64* %a_0
        store i64 10, i64* %a_0, align 8
        ret void
      }
      |]

  it "supports 'phi' instruction" $ do
    let ir = do
          function "func" [(i32, "a"), (i32, "b")] i32 $ \[a, b] -> mdo
            c <- icmp EQ a b
            condBr c block1 block2

            block1 <- block
            br block3

            block2 <- block
            br block3

            block3 <- block
            d <- phi [(a, block1), (b, block2)]
            ret d
    checkIR ir [text|
      define external ccc i32 @func(i32 %a_0, i32 %b_0) {
      start:
        %0 = icmp eq i32 %a_0, %b_0
        br i1 %0, label %block_0, label %block_1
      block_0:
        br label %block_2
      block_1:
        br label %block_2
      block_2:
        %1 = phi i32 [%a_0, %block_0], [%b_0, %block_1]
        ret i32 %1
      }
      |]

  it "supports 'call' instruction" $ do
    let ir = mdo
          func <- function "func" [(i32, "a")] (i32) $ \[a] -> do
            ret =<< call func [a]

          pure ()
    checkIR ir [text|
      define external ccc i32 @func(i32 %a_0) {
      start:
        %0 = call ccc i32 @func(i32 %a_0)
        ret i32 %0
      }
      |]

  it "supports 'ret' instruction" $ do
    let ir = do
          function "func" [(i1, "a")] i1 $ \[a] -> do
            ret a
    checkIR ir [text|
      define external ccc i1 @func(i1 %a_0) {
      start:
        ret i1 %a_0
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

  it "only uses first terminator instruction" $ do
    let ir = do
          function "func" [] i1 $ \[] -> do
            ret (bit 0)
            ret (bit 1)
    checkIR ir [text|
      define external ccc i1 @func() {
      start:
        ret i1 0
      }
      |]

  it "doesn't emit a block if it has no instructions or terminator" $ do
    let ir = do
          function "func" [(i32, "a"), (i32, "b")] i32 $ \[a, b] -> mdo
            isZero <- eq a (int32 0)
            if' isZero $ do
              _ <- add a b
              ret $ int32 1000
              br blk

            blk <- block
            ret b
    checkIR ir [text|
      define external ccc i32 @func(i32 %a_0, i32 %b_0) {
      start:
        %0 = icmp eq i32 %a_0, 0
        br i1 %0, label %if_0, label %end_if_0
      if_0:
        %1 = add i32 %a_0, %b_0
        ret i32 1000
      end_if_0:
        br label %block_0
      block_0:
        ret i32 %b_0
      }
      |]

  it "supports 'br' instruction" $ do
    let ir = do
          function "func" [(i1, "a")] i1 $ \[a] -> mdo
            br block2

            block1 <- block
            ret a

            block2 <- block
            br block1
    checkIR ir [text|
      define external ccc i1 @func(i1 %a_0) {
      start:
        br label %block_1
      block_0:
        ret i1 %a_0
      block_1:
        br label %block_0
      }
      |]

  it "supports 'condBr' instruction" $ do
    let ir = do
          function "func" [(i1, "a")] i1 $ \[a] -> mdo
            condBr a block1 block2

            block1 <- block
            ret a

            block2 <- block
            condBr a block1 block3

            block3 <- block
            condBr a block1 block2
    checkIR ir [text|
      define external ccc i1 @func(i1 %a_0) {
      start:
        br i1 %a_0, label %block_0, label %block_1
      block_0:
        ret i1 %a_0
      block_1:
        br i1 %a_0, label %block_0, label %block_2
      block_2:
        br i1 %a_0, label %block_0, label %block_1
      }
      |]

  it "supports 'switch' instruction" $ do
    let ir = do
          function "func" [(i1, "a")] i1 $ \[a] -> mdo
            switch a defaultBlock [(bit 1, block1), (bit 0, block2)]
            block1 <- block
            ret a
            block2 <- block
            ret a
            defaultBlock <- block
            ret a
    checkIR ir [text|
      define external ccc i1 @func(i1 %a_0) {
      start:
        switch i1 %a_0, label %block_2 [i1 1, label %block_0 i1 0, label %block_1]
      block_0:
        ret i1 %a_0
      block_1:
        ret i1 %a_0
      block_2:
        ret i1 %a_0
      }
      |]


  it "supports 'select' instruction" $ do
    let ir = do
          function "not" [(i1, "a")] i1 $ \[a] -> do
            b <- select a (bit 0) (bit 1)
            ret b
    checkIR ir [text|
      define external ccc i1 @not(i1 %a_0) {
      start:
        %0 = select i1 %a_0, i1 0, i1 1
        ret i1 %0
      }
      |]

  it "supports 'bit' for creating i1 values" $ do
    let ir = do
          function "func" [] i1 $ \[] -> do
            ret (bit 1)
    checkIR ir [text|
      define external ccc i1 @func() {
      start:
        ret i1 1
      }
      |]
    let ir2 = do
          function "func" [] i1 $ \[] -> do
            ret (bit 0)
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

  it "supports 'nullPtr' for creating null values" $ do
    let ir = do
          function "func" [] (ptr i8) $ \[] -> do
            ret $ nullPtr i8
    checkIR ir [text|
      define external ccc i8* @func() {
      start:
        ret i8* zeroinitializer
      }
      |]

  describe "function attributes" $ parallel $ do
    let checkAttr attr attrStr =
          it ("supports " <> T.unpack attrStr) $ do
            let ir = withFunctionAttributes (const [attr]) $
                       function "func" [] (IntType 42) $ \[] -> do
                         ret (intN 42 1000)
            checkIR ir [text|
              define external ccc i42 @func() $attrStr {
              start:
                ret i42 1000
              }
              |]

    checkAttr AlwaysInline "alwaysinline"
    checkAttr (WasmExportName "test") "\"wasm-export-name\"=\"test\""

    it "supports multiple function attributes" $ do
      let attrs = [AlwaysInline, WasmExportName "test"]
          ir = withFunctionAttributes (const attrs) $
                 function "func" [] (IntType 42) $ \[] -> do
                   ret (intN 42 1000)
      checkIR ir [text|
        define external ccc i42 @func() alwaysinline "wasm-export-name"="test" {
        start:
          ret i42 1000
        }
        |]
