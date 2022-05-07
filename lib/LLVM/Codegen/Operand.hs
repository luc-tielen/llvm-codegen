module LLVM.Codegen.Operand
  ( Operand(..)
  , typeOf
  ) where

import LLVM.NameSupply (Name)
import LLVM.Codegen.Type


data Operand
  = LocalRef Type Name
  | GlobalRef Type Name
  deriving Show

typeOf :: Operand -> Type
typeOf = \case
  LocalRef ty _ -> ty
  GlobalRef ty _ -> ty
