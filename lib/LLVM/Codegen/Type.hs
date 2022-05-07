module LLVM.Codegen.Type
  ( Type(..)
  ) where

data Type
  = IntType Int
  | FunctionType Type [Type]
  | PointerType Type
  deriving Show
