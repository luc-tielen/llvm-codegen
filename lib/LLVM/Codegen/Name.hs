module LLVM.Codegen.Name
  ( Name(..)
  , renderName
  ) where

import Data.Text
import Data.String
import LLVM.Pretty

newtype Name = Name { unName :: Text }
  deriving (Eq, Ord, Show)

instance IsString Name where
  fromString = Name . fromString

renderName :: Renderer Name
renderName buf (Name name) =
  buf |> name
{-# INLINABLE renderName #-}
