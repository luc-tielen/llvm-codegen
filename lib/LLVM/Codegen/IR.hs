module LLVM.Codegen.IR
  ( IR(..)
  , Terminator(..)
  ) where

import LLVM.NameSupply  -- TODO: separate import for name
import LLVM.Codegen.Operand
import LLVM.Codegen.Type
import LLVM.Pretty

type NUW = Bool
type NSW = Bool
type Exact = Bool

data IR
  = Add NUW NSW Operand Operand
  | Mul NUW NSW Operand Operand
  | Sub NUW NSW Operand Operand
  | Udiv Exact Operand Operand
  | And Operand Operand
  | Trunc Operand Type
  | Zext Operand Type
  | Bitcast Operand Type
  | PtrToInt Operand Type
  -- Terminators
  | Ret (Maybe Operand)
  | Br Name
  | CondBr Operand Name Name
  | Switch Type Operand Name [(Type, Operand, Name)]
  deriving Show

newtype Terminator
  = Terminator IR
  deriving Show
-- TODO use pattern synonyms for Terminator?

instance Pretty IR where
  pretty = \case
    Add nuw nsw a b ->
      prettyArithBinOp "add" nuw nsw a b
    Mul nuw nsw a b ->
      prettyArithBinOp "mul" nuw nsw a b
    Sub nuw nsw a b ->
      prettyArithBinOp "sub" nuw nsw a b
    Udiv exact a b ->
      "udiv" <+> prettyFlag "exact" exact <> pretty (typeOf a) <+> pretty a <> "," <+> pretty b
    And a b ->
      "and" <+> pretty (typeOf a) <+> pretty a <> "," <+> pretty b
    Trunc val to ->
      prettyConvertOp "trunc" val to
    Zext val to ->
      prettyConvertOp "zext" val to
    Bitcast val to ->
      prettyConvertOp "bitcast" val to
    PtrToInt val to ->
      prettyConvertOp "ptrtoint" val to
    Ret term ->
      case term of
        Nothing ->
          "ret void"
        Just operand ->
          "ret " <> pretty (typeOf operand) <+> pretty operand
    Br blockName ->
      "br label" <+> pretty blockName
    CondBr cond trueLabel falseLabel ->
      "br i1" <+> pretty cond <> ", label" <+> pretty trueLabel <+> ", label" <+> pretty falseLabel
    Switch ty val defaultLabel cases ->
      "switch" <+> pretty ty <+> pretty val <> "," <+> pretty defaultLabel <+>
        list (map prettyCase cases)
      where
        prettyCase (caseTy, caseVal, label) =
          pretty caseTy <+> pretty caseVal <> ", label" <+> pretty label

prettyArithBinOp :: Doc ann -> Bool -> Bool -> Operand -> Operand -> Doc ann
prettyArithBinOp opName nuw nsw a b =
  opName <+> prettyFlag "nuw" nuw <> prettyFlag "nsw" nsw <> pretty (typeOf a) <+> pretty a <> "," <+> pretty b

prettyConvertOp :: Doc ann -> Operand -> Type -> Doc ann
prettyConvertOp opName val to =
  opName <+> pretty (typeOf val) <+> "to" <+> pretty to

prettyFlag :: Doc ann -> Bool -> Doc ann
prettyFlag doc = \case
  False -> ""
  True -> doc <> " "
