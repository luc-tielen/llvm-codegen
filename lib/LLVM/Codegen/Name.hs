module LLVM.Codegen.Name
  ( Name(..)
  ) where

import Data.Text
import Data.String
import LLVM.Pretty

newtype Name = Name { unName :: Text }
  deriving (Eq, Ord, Show)

instance IsString Name where
  fromString = Name . fromString

instance Pretty Name where
  pretty (Name name) =
    pretty name
