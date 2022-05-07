{-# LANGUAGE OverloadedStrings, RecursiveDo, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-orphans #-}

module LLVM.Codegen
  ( module LLVM.Codegen  -- TODO clean up exports
  ) where

import qualified Data.DList as DList
import Data.Text (Text)
import qualified Data.Text as T
import LLVM.NameSupply
import LLVM.Codegen.IRBuilder
import LLVM.Codegen.ModuleBuilder
import LLVM.Codegen.Operand
import LLVM.Codegen.IR
import LLVM.Codegen.Type
import Prettyprinter
import Prettyprinter.Render.Text

exampleModule :: Module
exampleModule = runModuleBuilder $ do
  function (Name "do_add") [IntType 1, IntType 32] (IntType 8) $ \[x, y] -> mdo
    z <- add x y
    _ <- add x y

    _ <- block
    _ <- add y z
    ret y


-- TODO move to separate module
renderDoc :: Pretty a => a -> Text
renderDoc =
  renderStrict . layoutSmart defaultLayoutOptions . pretty

instance Pretty Module where
  pretty (Module defs) =
    vsep $ map pretty defs

-- TODO: fix orphan instances

instance Pretty Name where
  pretty (Name name) =
    pretty name

instance Pretty Type where
  pretty = \case
    PointerType ty -> pretty ty <> "*"
    IntType bits   -> "i" <> pretty bits
    FunctionType retTy argTys -> pretty retTy <+> tupled (map pretty argTys)

instance Pretty Definition where
 pretty = \case
   Function name retTy argTys body ->
     "define external ccc" <+> pretty retTy <+> fnName <> tupled (zipWith prettyArg [0..] argTys) <+>
       "{" <> hardline <>
       prettyBody body <> hardline <>
       "}"
    where
      fnName = "@" <> pretty name
      prettyArg i argTy = pretty $ LocalRef argTy $ Name $ T.pack (show i)
      prettyBody blocks = vsep $ map pretty blocks

instance Pretty BasicBlock where
  pretty (BB (Name name) stmts (Terminator term)) =
    let prettyStmts = indent 2 $ vsep $ (map (uncurry prettyStmt) $ DList.apply stmts []) ++ [pretty term]
     in vsep [ pretty name <> ":", prettyStmts ]
    where
      prettyStmt operand instr =
        pretty operand <+> "=" <+> pretty instr

instance Pretty IR where
  pretty = \case
    Add a b ->
      "add" <+> pretty (typeOf a) <+> pretty a <+> pretty b
    Ret term ->
      case term of
        Nothing ->
          "ret void"
        Just operand ->
          "ret " <> pretty (typeOf operand) <+> pretty operand

instance Pretty Operand where
  pretty = \case
    LocalRef _ name -> "%" <> pretty name
    GlobalRef _ name -> "@" <> pretty name

-- $> Data.Text.IO.putStrLn $ renderDoc exampleModule
