{-# LANGUAGE RecursiveDo, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults#-}

module LLVM.Codegen
  ( module LLVM.Codegen  -- TODO clean up exports
  ) where

import qualified Data.DList as DList
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Foldable
import LLVM.NameSupply
import LLVM.Codegen.IRBuilder
import LLVM.Codegen.ModuleBuilder
import LLVM.Codegen.Operand
import LLVM.Codegen.IR
import LLVM.Codegen.Type


exampleModule :: [Definition]
exampleModule = runModuleBuilder $ do
  function (Name "do_add") [IntType 1, IntType 32] (IntType 8) $ \[x, y] -> mdo
    z <- add x y
    _ <- add x y

    _ <- block
    _ <- add y z
    ret y


pp :: [Definition] -> Text
pp defs =
  T.unlines $ map ppDefinition defs

ppDefinition :: Definition -> Text
ppDefinition = \case
  Function (Name name) retTy argTys body ->
    "define external ccc " <> ppType retTy <> " @" <> name <> "(" <> fold (L.intersperse ", " (zipWith ppArg [0..] argTys)) <> ")" <> "{\n" <>
      ppBody body <> "\n" <>
      "}"
    where
      ppArg i argTy = ppOperand $ LocalRef argTy $ Name $ T.pack (show i)
      ppBody blocks = T.unlines $ map ppBasicBlock blocks

ppBasicBlock :: BasicBlock -> Text
ppBasicBlock (BB (Name name) stmts (Terminator term)) =
  T.unlines
  [ name <> ":"
  , T.unlines $ map (uncurry ppStmt) $ DList.apply stmts []
  , ppInstr term
  ]
  where
    ppStmt operand instr = ppOperand operand <> " = " <> ppInstr instr

ppInstr :: IR -> Text
ppInstr = \case
  Add a b -> "add " <> ppType (typeOf a) <> " " <> ppOperand a <> " " <> ppOperand b
  Ret term ->
    case term of
      Nothing ->
        "ret void"
      Just operand ->
        "ret " <> ppType (typeOf operand) <> " " <> ppOperand operand

ppType :: Type -> Text
ppType = \case
  PointerType ty ->
    ppType ty <> "*"
  IntType bits ->
    "i" <> T.pack (show bits)
  FunctionType retTy argTys ->
    ppType retTy <> " " <> fold (L.intersperse ", " $ map ppType argTys)

ppOperand :: Operand -> Text
ppOperand op = "%" <> unName opName
  where
    opName = case op of
      LocalRef _ name -> name
      GlobalRef _ name -> name

-- $> putStrLn . Data.Text.unpack $ pp exampleModule
