module LLVM.Codegen.Type
  ( Type(..)
  ) where

import LLVM.Pretty

data Type
  = IntType Int
  | FunctionType Type [Type]
  | PointerType Type
  deriving Show


instance Pretty Type where
  pretty = \case
    PointerType ty -> pretty ty <> "*"
    IntType bits   -> "i" <> pretty bits
    FunctionType retTy argTys -> pretty retTy <+> tupled (map pretty argTys)
