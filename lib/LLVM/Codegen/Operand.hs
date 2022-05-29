module LLVM.Codegen.Operand
  ( Operand(..)
  , Constant(..)
  , typeOf
  ) where

import LLVM.Codegen.Name
import LLVM.Codegen.Type
import LLVM.Pretty
import Data.Word


data Constant
  = GlobalRef Type Name
  | Int Word32 Integer
  | NullPtr Type
  | Undef Type
  deriving (Eq, Ord, Show)

data Operand
  = LocalRef Type Name
  | ConstantOperand Constant
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
      Int bits _ ->
        IntType bits
      NullPtr ty ->
        ptr ty
      Undef ty ->
        ty

instance Pretty Operand where
  pretty = \case
    LocalRef _ name ->
      "%" <> pretty name
    ConstantOperand c ->
      pretty c

instance Pretty Constant where
  pretty = \case
    GlobalRef _ name ->
      "@" <> pretty name
    Int _bits x ->
      pretty x
    NullPtr _ ->
      "zeroinitializer"
    Undef _ ->
      "undef"
