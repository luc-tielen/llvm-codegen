module LLVM.Pretty
  ( renderDoc
  , module Prettyprinter
  ) where

import Prettyprinter
import Prettyprinter.Render.Text
import Data.Text (Text)

renderDoc :: Pretty a => a -> Text
renderDoc =
  renderStrict . layoutSmart defaultLayoutOptions . pretty
