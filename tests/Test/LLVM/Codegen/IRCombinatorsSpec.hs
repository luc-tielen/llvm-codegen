{-# LANGUAGE QuasiQuotes, RecursiveDo, OverloadedLists #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.LLVM.Codegen.IRCombinatorsSpec
  ( module Test.LLVM.Codegen.IRCombinatorsSpec
  ) where

import Test.Hspec
import Data.Foldable (for_)
import LLVM.Codegen
import Data.Text (Text)
import NeatInterpolation

checkIR :: ModuleBuilder a -> Text -> IO ()
checkIR llvmModule expectedOutput = do
  let ir = ppllvm $ runModuleBuilder llvmModule
  ir `shouldBe` expectedOutput

spec :: Spec
spec = describe "IR builder combinators" $ parallel $ do
  it "supports comparisons combinators" $ do
    let scenarios :: [(Operand -> Operand -> IRBuilderT ModuleBuilder Operand, Text)]
        scenarios = [ (eq, "eq"), (ne, "ne")
                    , (sge, "sge"), (sgt, "sgt"), (slt, "slt"), (sle, "sle")
                    , (uge, "uge"), (ugt, "ugt"), (ult, "ult"), (ule, "ule")
                    ]
    for_ scenarios $ \(f, op) -> do
      let ir = do
            function "func" [(i32, "a"), (i32, "b")] i1 $ \[a, b] -> do
              c <- f a b
              ret c
      checkIR ir [text|
        define external ccc i1 @func(i32 %a_0, i32 %b_0) {
        start:
          %0 = icmp $op i32 %a_0, %b_0
          ret i1 %0
        }
        |]

  it "supports 'one-sided if' combinator" $ do
    let ir = do
          function "func" [(i32, "a"), (i32, "b")] i32 $ \[a, b] -> mdo
            isZero <- eq a (int32 0)
            if' isZero $ do
              _ <- add a b
              ret $ int32 1000

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
        ret i32 %b_0
      }
      |]

  it "supports 'loop' combinator" $ do
    let ir = do
          function "func" [] i32 $ \_ -> mdo
            i <- allocate i32 (int32 0)

            loop $ do
              iValue <- load i 0
              isEqual <- iValue `eq` int32 10
              if' isEqual $ do
                br end

            end <- blockNamed "end"
            ret $ int32 42
    checkIR ir [text|
      define external ccc i32 @func() {
      start:
        %stack.ptr_0 = alloca i32
        store i32 0, i32* %stack.ptr_0
        br label %loop_0
      loop_0:
        %0 = load i32, i32* %stack.ptr_0
        %1 = icmp eq i32 %0, 10
        br i1 %1, label %if_0, label %end_if_0
      if_0:
        br label %end_0
      end_if_0:
        br label %loop_0
      end_0:
        ret i32 42
      }
      |]

  it "supports 'loopWhile' combinator" $ do
    let ir = do
          function "func" [] i32 $ \_ -> mdo
            i <- allocate i32 (int32 10)
            let notZero = do
                  iVal <- load i 0
                  iVal `ne` int32 0
            loopWhile notZero $ do
              iVal <- load i 0
              iVal' <- sub iVal (int32 1)
              store i 0 iVal'

            ret $ int32 42
    checkIR ir [text|
      define external ccc i32 @func() {
      start:
        %stack.ptr_0 = alloca i32
        store i32 10, i32* %stack.ptr_0
        br label %while_begin_0
      while_begin_0:
        %0 = load i32, i32* %stack.ptr_0
        %1 = icmp ne i32 %0, 0
        br i1 %1, label %while_body_0, label %while_end_0
      while_body_0:
        %2 = load i32, i32* %stack.ptr_0
        %3 = sub i32 %2, 1
        store i32 %3, i32* %stack.ptr_0
        br label %while_begin_0
      while_end_0:
        ret i32 42
      }
      |]

  it "supports 'loopFor' combinator" $ do
    let ir = do
          function "func" [] i32 $ \_ -> mdo
            x <- allocate i32 (int32 10)

            loopFor (int32 0) (`ult` int32 10) (add (int32 1)) $ \i -> do
              xVal <- load x 0
              xVal' <- add i xVal
              store x 0 xVal'

            ret $ int32 42
    checkIR ir [text|
      define external ccc i32 @func() {
      start:
        %stack.ptr_0 = alloca i32
        store i32 10, i32* %stack.ptr_0
        br label %for_begin_0
      for_begin_0:
        %0 = phi i32 [0, %start], [%4, %for_body_0]
        %1 = icmp ult i32 %0, 10
        br i1 %1, label %for_body_0, label %for_end_0
      for_body_0:
        %2 = load i32, i32* %stack.ptr_0
        %3 = add i32 %0, %2
        store i32 %3, i32* %stack.ptr_0
        %4 = add i32 1, %0
        br label %for_begin_0
      for_end_0:
        ret i32 42
      }
      |]

  it "supports 'pointer subtraction' combinator" $ do
    let ir = do
          function "func" [] i32 $ \_ -> mdo
            array <- alloca i32 (Just $ int32 5) 0
            ptr1 <- gep array [int32 0]
            ptr2 <- gep array [int32 3]
            _ <- pointerDiff i32 ptr1 ptr2
            ret $ int32 42
    checkIR ir [text|
      define external ccc i32 @func() {
      start:
        %stack.ptr_0 = alloca i32, i32 5
        %0 = getelementptr i32, i32* %stack.ptr_0, i32 0
        %1 = getelementptr i32, i32* %stack.ptr_0, i32 3
        %2 = ptrtoint i32* %0 to i64
        %3 = ptrtoint i32* %1 to i64
        %4 = sub i64 %2, %3
        %5 = trunc i64 %4 to i32
        ret i32 42
      }
      |]

  it "supports logical not" $ do
    let ir = do
          function "func" [] i32 $ \_ -> mdo
            _ <- not' $ bit 0
            ret $ int32 42
    checkIR ir [text|
      define external ccc i32 @func() {
      start:
        %0 = select i1 0, i1 0, i1 1
        ret i32 42
      }
      |]

  it "supports computing the minimum of 2 values" $ do
    let ir = do
          function "func" [] i32 $ \_ -> mdo
            _result1 <- minimum' Signed (int32 100) (int32 42)
            _result2 <- minimum' Unsigned (int32 100) (int32 42)
            ret $ int32 42
    checkIR ir [text|
      define external ccc i32 @func() {
      start:
        %0 = icmp slt i32 100, 42
        %1 = select i1 %0, i32 100, i32 42
        %2 = icmp ult i32 100, 42
        %3 = select i1 %2, i32 100, i32 42
        ret i32 42
      }
      |]

  it "supports allocating and initializing a variable on the stack" $ do
    let ir = do
          function "func" [] i32 $ \_ -> mdo
            _i <- allocate i32 (int32 0)
            ret $ int32 42
    checkIR ir [text|
      define external ccc i32 @func() {
      start:
        %stack.ptr_0 = alloca i32
        store i32 0, i32* %stack.ptr_0
        ret i32 42
      }
      |]

  it "supports composing Paths" $ do
    let path = mkPath [int32 1, int32 2] ->> mkPath [int32 3]
    path `shouldBe` Path [int32 0, int32 1, int32 2, int32 3]

  it "supports computing the address based on a Path" $ do
    let path = Path [int32 5]
        ir = do
          function "func" [] i32 $ \_ -> mdo
            array <- alloca i32 (Just $ int32 5) 0
            _address <- addr path array
            ret $ int32 42
    checkIR ir [text|
      define external ccc i32 @func() {
      start:
        %stack.ptr_0 = alloca i32, i32 5
        %0 = getelementptr i32, i32* %stack.ptr_0, i32 5
        ret i32 42
      }
      |]

  it "supports dereferencing an address based on a Path" $ do
    let path = Path [int32 5]
        ir = mdo
          function "func" [] i32 $ \_ -> mdo
            array <- alloca i32 (Just $ int32 5) 0
            _value <- deref path array
            ret $ int32 42
    checkIR ir [text|
      define external ccc i32 @func() {
      start:
        %stack.ptr_0 = alloca i32, i32 5
        %0 = getelementptr i32, i32* %stack.ptr_0, i32 5
        %1 = load i32, i32* %0
        ret i32 42
      }
      |]

  it "supports storing a value at an address based on a Path" $ do
    let path = Path [int32 5]
        ir = mdo
          function "func" [] i32 $ \_ -> mdo
            array <- alloca i32 (Just $ int32 5) 0
            assign path array (int32 1000)
            ret $ int32 42
    checkIR ir [text|
      define external ccc i32 @func() {
      start:
        %stack.ptr_0 = alloca i32, i32 5
        %0 = getelementptr i32, i32* %stack.ptr_0, i32 5
        store i32 1000, i32* %0
        ret i32 42
      }
      |]

  it "supports updating a value at an address based on a Path" $ do
    let path = Path [int32 5]
        ir = mdo
          function "func" [] i32 $ \_ -> mdo
            array <- alloca i32 (Just $ int32 5) 0
            assign path array (int32 1000)
            update path array (add (int32 10))
            ret $ int32 42
    checkIR ir [text|
      define external ccc i32 @func() {
      start:
        %stack.ptr_0 = alloca i32, i32 5
        %0 = getelementptr i32, i32* %stack.ptr_0, i32 5
        store i32 1000, i32* %0
        %1 = getelementptr i32, i32* %stack.ptr_0, i32 5
        %2 = load i32, i32* %1
        %3 = add i32 10, %2
        store i32 %3, i32* %1
        ret i32 42
      }
      |]

  it "supports incrementing a value at an address based on a Path" $ do
    let path = Path [int32 5]
        ir = mdo
          function "func" [] i32 $ \_ -> mdo
            array <- alloca i32 (Just $ int32 5) 0
            assign path array (int32 1000)
            increment int32 path array
            ret $ int32 42
    checkIR ir [text|
      define external ccc i32 @func() {
      start:
        %stack.ptr_0 = alloca i32, i32 5
        %0 = getelementptr i32, i32* %stack.ptr_0, i32 5
        store i32 1000, i32* %0
        %1 = getelementptr i32, i32* %stack.ptr_0, i32 5
        %2 = load i32, i32* %1
        %3 = add i32 1, %2
        store i32 %3, i32* %1
        ret i32 42
      }
      |]

  it "supports copying (part of) a type based on a Path" $ do
    let path = Path [int32 5]
        ir = mdo
          function "func" [] i32 $ \_ -> mdo
            array <- alloca i32 (Just $ int32 5) 0
            assign path array (int32 1000)
            array2 <- alloca i32 (Just $ int32 5) 0
            copy path array array2
            ret $ int32 42
    checkIR ir [text|
      define external ccc i32 @func() {
      start:
        %stack.ptr_0 = alloca i32, i32 5
        %stack.ptr_1 = alloca i32, i32 5
        %0 = getelementptr i32, i32* %stack.ptr_0, i32 5
        store i32 1000, i32* %0
        %1 = getelementptr i32, i32* %stack.ptr_0, i32 5
        %2 = load i32, i32* %1
        %3 = getelementptr i32, i32* %stack.ptr_1, i32 5
        store i32 %2, i32* %3
        ret i32 42
      }
      |]

  it "supports swapping (part of) a type based on a Path" $ do
    let path = Path [int32 5]
        ir = mdo
          function "func" [] i32 $ \_ -> mdo
            array <- alloca i32 (Just $ int32 5) 0
            assign path array (int32 1000)
            array2 <- alloca i32 (Just $ int32 5) 0
            swap path array array2
            ret $ int32 42
    checkIR ir [text|
      define external ccc i32 @func() {
      start:
        %stack.ptr_0 = alloca i32, i32 5
        %stack.ptr_1 = alloca i32, i32 5
        %0 = getelementptr i32, i32* %stack.ptr_0, i32 5
        store i32 1000, i32* %0
        %1 = getelementptr i32, i32* %stack.ptr_0, i32 5
        %2 = load i32, i32* %1
        %3 = getelementptr i32, i32* %stack.ptr_1, i32 5
        %4 = load i32, i32* %3
        %5 = getelementptr i32, i32* %stack.ptr_0, i32 5
        store i32 %4, i32* %5
        %6 = getelementptr i32, i32* %stack.ptr_1, i32 5
        store i32 %2, i32* %6
        ret i32 42
      }
      |]

