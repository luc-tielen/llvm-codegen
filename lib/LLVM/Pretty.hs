module LLVM.Pretty
  ( renderDoc
  , commas
  , module Prettyprinter
  ) where

import Prettyprinter
import Prettyprinter.Render.Text
import Data.Text (Text)
import qualified Data.List as L

renderDoc :: Pretty a => a -> Text
renderDoc =
  renderStrict . layoutSmart defaultLayoutOptions . pretty

commas :: [Doc ann] -> Doc ann
commas docs =
  mconcat $ L.intersperse ", " docs
