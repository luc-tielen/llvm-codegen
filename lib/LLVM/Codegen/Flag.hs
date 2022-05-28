{-# LANGUAGE RoleAnnotations #-}

module LLVM.Codegen.Flag
  ( Flag(..)
  ) where

data Flag a
  = On
  | Off
  deriving (Eq, Ord, Show)

type role Flag nominal
