module LLVM.Codegen.Operand
  ( Operand(..)
  , Constant(..)
  , typeOf
  , renderOperand
  , renderConstant
  ) where

import LLVM.Codegen.Name
import LLVM.Codegen.Type
import LLVM.Pretty
import Data.Word


data Constant
  = GlobalRef !Type !Name
  | Array !Type ![Constant]
  | Int !Word32 !Integer
  | NullPtr !Type
  | Undef !Type
  deriving (Eq, Ord, Show)

data Operand
  = LocalRef !Type !Name
  | ConstantOperand !Constant
  deriving (Eq, Ord, Show)

typeOf :: Operand -> Type
typeOf = \case
  LocalRef ty _ ->
    ty
  ConstantOperand c ->
    typeOfConstant c
  where
    typeOfConstant = \case
      GlobalRef ty _ ->
        ty
      Array ty cs ->
        ArrayType (fromIntegral $ length cs) ty
      Int bits _ ->
        IntType bits
      NullPtr ty ->
        ptr ty
      Undef ty ->
        ty
{-# INLINEABLE typeOf #-}

renderConstant :: Renderer Constant
renderConstant buf = \case
  GlobalRef _ name ->
    (buf |>. '@') `renderName` name
  Array ty cs ->
    brackets buf (\buf' -> commas buf' cs renderValue)
    where
      renderValue ::  Renderer Constant
      renderValue buf' c = (renderType buf' ty  |>. ' ') `renderConstant` c
  Int _bits x ->
    buf |>$ (fromInteger x :: Int)
  NullPtr _ ->
    buf |># "zeroinitializer"#
  Undef _ ->
    buf |># "undef"#

renderOperand :: Renderer Operand
renderOperand buf = \case
  LocalRef _ name ->
    (buf |>. '%') `renderName` name
  ConstantOperand c ->
    renderConstant buf c
{-# INLINEABLE renderOperand #-}
