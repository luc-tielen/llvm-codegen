module LLVM.Codegen.IR
  ( IR(..)
  , Terminator(..)
  ) where

import LLVM.Codegen.Operand
import LLVM.Pretty


data IR
  = Add Operand Operand
  | Ret (Maybe Operand)
  deriving Show

newtype Terminator
  = Terminator IR
  deriving Show


instance Pretty IR where
  pretty = \case
    Add a b ->
      "add" <+> pretty (typeOf a) <+> pretty a <+> pretty b
    Ret term ->
      case term of
        Nothing ->
          "ret void"
        Just operand ->
          "ret " <> pretty (typeOf operand) <+> pretty operand

