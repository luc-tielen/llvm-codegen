module LLVM.Codegen.IR
  ( IR(..)
  , Terminator(..)
  ) where

import LLVM.Codegen.Operand


data IR
  = Add Operand Operand
  | Ret (Maybe Operand)
  deriving Show

newtype Terminator
  = Terminator IR
  deriving Show

