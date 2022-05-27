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
  deriving (Eq, Show)

data Operand
  = LocalRef Type Name
  | ConstantOperand Constant
  deriving (Eq, Show)

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
