module LLVM.Pretty
  ( Renderer
  , renderDoc
  , (|>)
  , (|>.)
  , (|>#)
  , (|>$)
  , Buffer
  , Addr#
  , runBuffer
  , consumeBuffer
  , hsep
  , vsep
  , sepBy
  , brackets
  , braces
  , parens
  , commas
  , dquotes
  , tupled
  , withIndent
  , optional
  , renderMaybe
  ) where

import Prelude hiding (EQ)
import Data.Text.Builder.Linear.Buffer
import Data.Text (Text)
import qualified Data.List as L
import GHC.Prim (Addr#)
import LLVM.Codegen.Flag

type Renderer a = Buffer %1 -> a -> Buffer

renderDoc :: Renderer a -> a -> Text
renderDoc f d =
  runBuffer (`f` d)
{-# INLINABLE renderDoc #-}

-- TODO better name
type BufferDecorator = Buffer %1 -> (Buffer %1 -> Buffer) -> Buffer

brackets :: BufferDecorator
brackets = betweenChars '[' ']'
{-# INLINABLE brackets #-}

braces :: BufferDecorator
braces = betweenChars '{' '}'
{-# INLINABLE braces #-}

parens :: BufferDecorator
parens = betweenChars '(' ')'
{-# INLINABLE parens #-}

dquotes :: BufferDecorator
dquotes = betweenChars '"' '"'
{-# INLINABLE dquotes #-}

betweenChars :: Char -> Char -> BufferDecorator
betweenChars begin end buf f =
  f (buf |>. begin) |>. end
{-# INLINABLE betweenChars #-}

withIndent :: BufferDecorator
withIndent buf' f =
  f (buf' |># "  "#)
{-# INLINABLE withIndent #-}

hsep :: Buffer %1 -> [a] -> Renderer a -> Buffer
hsep = sepBy " "#
{-# INLINABLE hsep #-}

vsep :: Buffer %1 -> [a] -> Renderer a -> Buffer
vsep = sepBy "\n"#
{-# INLINABLE vsep #-}

tupled :: Buffer %1 -> [a] -> Renderer a -> Buffer
tupled buf as f =
  parens buf (\buf' -> commas buf' as f)
{-# INLINABLE tupled #-}

commas :: Buffer %1 -> [a] -> Renderer a -> Buffer
commas = sepBy ", "#
{-# INLINABLE commas #-}

sepBy :: forall a. Addr# -> Buffer %1 -> [a] -> Renderer a -> Buffer
sepBy separator buf as f =
  foldlIntoBuffer combine buf parts
  where
    parts = L.intersperse Nothing $ map Just as
    combine :: Renderer (Maybe a)
    combine buf' = \case
      Nothing ->
        buf' |># separator
      Just a ->
        f buf' a
{-# INLINABLE sepBy #-}

optional :: Flag a -> BufferDecorator
optional flag buf f = case flag of
  Off -> buf
  On -> f buf
{-# INLINABLE optional #-}

renderMaybe :: Buffer %1 -> Maybe a -> Renderer a -> Buffer
renderMaybe buf mValue render =
  case mValue of
    Nothing ->
      buf
    Just value ->
      render buf value
{-# INLINABLE renderMaybe #-}
