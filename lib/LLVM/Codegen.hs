module LLVM.Codegen
  ( module LLVM.Codegen.IRBuilder
  , module LLVM.Codegen.ModuleBuilder
  , module LLVM.Codegen.Type
  , module LLVM.Codegen.Operand
  , module LLVM.Codegen.Name
  , module LLVM.Codegen.IR
  , module LLVM.Codegen.Flag
  , ppllvm
  ) where

import LLVM.Codegen.IRBuilder
import LLVM.Codegen.ModuleBuilder
import LLVM.Codegen.Type
import LLVM.Codegen.Operand
import LLVM.Codegen.Name
import LLVM.Codegen.IR
import LLVM.Codegen.Flag
import LLVM.Pretty
import Data.Text


ppllvm :: Module -> Text
ppllvm = renderDoc renderModule
{-# INLINABLE ppllvm #-}
