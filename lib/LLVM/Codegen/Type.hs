module LLVM.Codegen.Type
  ( Type(..)
  , Packed
  , i1
  , i8
  , i16
  , i32
  , i64
  , ptr
  , void
  , renderType
  ) where

import LLVM.Codegen.Name
import LLVM.Codegen.Flag
import Data.Word
import LLVM.Pretty

data Packed

data Type
  = IntType !Word32
  | FunctionType !Type ![Type]
  | PointerType !Type
  | VoidType
  | StructureType !(Flag Packed) ![Type]
  | ArrayType !Word32 !Type
  | NamedTypeReference !Name
  deriving (Eq, Ord, Show)

i1, i8, i16, i32, i64 :: Type
i1 = IntType 1
i8 = IntType 8
i16 = IntType 16
i32 = IntType 32
i64 = IntType 64

ptr :: Type -> Type
ptr = PointerType

void :: Type
void = VoidType

renderType :: Renderer Type
renderType buf = \case
  PointerType _ ->
    buf |># "ptr"#
  IntType bits ->
    buf |>. 'i' |>$ bits
  FunctionType retTy argTys ->
    tupled (renderType buf retTy |>. ' ') argTys renderType
  NamedTypeReference name ->
    (buf |>. '%') `renderName` name
  VoidType ->
    buf |># "void"#
  StructureType packed elemTys
    | packed == On ->
      commas (buf |># "<{"#) elemTys renderType |># "}>"#
    | otherwise ->
      braces buf (\buf' -> commas buf' elemTys renderType)
  ArrayType count ty ->
    brackets buf (\buf' -> (buf' |>$ count |># " x "#) `renderType` ty)
{-# INLINABLE renderType #-}
