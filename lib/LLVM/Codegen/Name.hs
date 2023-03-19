module LLVM.Codegen.Name
  ( Name(..)
  , unName
  , renderName
  ) where

import Data.Text
import Data.String
import LLVM.Pretty

data Name
  = Generated !Int
  | Name !Text
  deriving (Eq, Ord, Show)

instance IsString Name where
  fromString = Name . fromString
  {-# INLINABLE fromString #-}

unName :: Name -> Text
unName = \case
  Name name -> name
  Generated x -> pack $! show x

renderName :: Renderer Name
renderName buf = \case
  Name name ->
    buf |> name
  Generated x ->
    buf |>$ x
{-# INLINABLE renderName #-}
