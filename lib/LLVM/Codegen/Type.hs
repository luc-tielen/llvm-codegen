module LLVM.Codegen.Type
  ( Type(..)
  , i1
  , i8
  , i16
  , i32
  , i64
  , ptr
  ) where

import LLVM.Pretty
import Data.Word

data Type
  = IntType Word32
  | FunctionType Type [Type]
  | PointerType Type
  deriving Show

i1, i8, i16, i32, i64 :: Type
i1 = IntType 1
i8 = IntType 8
i16 = IntType 16
i32 = IntType 32
i64 = IntType 64

ptr :: Type -> Type
ptr = PointerType

instance Pretty Type where
  pretty = \case
    PointerType ty ->
      pretty ty <> "*"
    IntType bits ->
      "i" <> pretty bits
    FunctionType retTy argTys ->
      pretty retTy <+> tupled (map pretty argTys)
