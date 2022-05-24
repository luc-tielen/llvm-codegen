module LLVM.Codegen.IR
  ( IR(..)
  , Terminator(..)
  , ComparisonType(..)
  , CallingConvention(..)
  , TailCallAttribute(..)
  , SynchronizationScope(..)
  , MemoryOrdering(..)
  , Alignment
  ) where

import Prelude hiding (EQ)
import LLVM.NameSupply  -- TODO: separate import for name
import LLVM.Codegen.Operand
import LLVM.Codegen.Type
import LLVM.Pretty
import Data.Word
import Data.List.NonEmpty (NonEmpty(..), toList)

-- TODO: create Flag datatype
type NUW = Bool
type NSW = Bool
type Exact = Bool
type Inbounds = Bool
type Volatile = Bool

type Alignment = Word32

data SynchronizationScope
  = SingleThread
  | System
  deriving Show

data MemoryOrdering
  = Unordered
  | Monotonic
  | Acquire
  | Release
  | AcquireRelease
  | SequentiallyConsistent
  deriving Show

type Atomicity = (SynchronizationScope, MemoryOrdering)

data ComparisonType
  = EQ
  | NE
  | UGT
  | UGE
  | ULT
  | ULE
  | SGT
  | SGE
  | SLT
  | SLE
  deriving (Eq, Show)

data TailCallAttribute
  = Tail | MustTail | NoTail
  deriving Show

data CallingConvention
  = C
  | Fast
  -- TODO add others as needed
  deriving Show

data IR
  = Add NUW NSW Operand Operand
  | Mul NUW NSW Operand Operand
  | Sub NUW NSW Operand Operand
  | Udiv Exact Operand Operand
  | And Operand Operand
  | Trunc Operand Type
  | Zext Operand Type
  | Bitcast Operand Type
  | ICmp ComparisonType Operand Operand
  | PtrToInt Operand Type
  | Alloca Type (Maybe Operand) Int
  | GetElementPtr Inbounds Operand [Operand]
  | Load Volatile Operand (Maybe Atomicity) Alignment
  | Store Volatile Operand Operand (Maybe Atomicity) Alignment
  | Phi (NonEmpty (Operand, Name))
  | Call (Maybe TailCallAttribute) CallingConvention Operand [Operand]  -- TODO support param attributes
  -- Terminators
  | Ret (Maybe Operand)
  | Br Name
  | CondBr Operand Name Name
  | Switch Operand Name [(Operand, Name)]
  | Select Operand Operand Operand
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
      "udiv" <+> optional exact "exact" <> pretty (typeOf a) <+> pretty a <> "," <+> pretty b
    And a b ->
      "and" <+> pretty (typeOf a) <+> pretty a <> "," <+> pretty b
    Trunc val to ->
      prettyConvertOp "trunc" val to
    Zext val to ->
      prettyConvertOp "zext" val to
    Bitcast val to ->
      prettyConvertOp "bitcast" val to
    ICmp cmp a b ->
      "icmp" <+> pretty cmp <+> pretty (typeOf a) <+> pretty a <> "," <+> pretty b
    PtrToInt val to ->
      prettyConvertOp "ptrtoint" val to
    Alloca ty numElems alignment ->
      "alloca" <+> pretty ty <+> maybeDoc numElems (\count -> "," <+> pretty (typeOf count) <+> pretty count)
                             <+> ", align" <+> pretty alignment
    GetElementPtr inbounds pointer indices ->
      case typeOf pointer of
        ty@(PointerType innerTy) ->
          "getelementptr" <+> optional inbounds "inbounds" <> pretty innerTy <> "," <> pretty ty <+>
            pretty pointer <> "," <+> (commas $ map prettyIndex indices)
        _ ->
          error "Operand given to `getelementptr` that is not a pointer!"
      where
        prettyIndex i = pretty (typeOf i) <+> pretty i
    Load volatile addr atomicity alignment ->
      let ptrTy = typeOf addr
          resultTy = case ptrTy of
            PointerType ty -> ty
            _ -> error "Malformed AST, expected pointer type."
          alignDoc = if alignment == 0 then mempty else ", align" <+> pretty alignment
       in case atomicity of
            Nothing ->
              "load" <+> optional volatile "volatile" <> pretty resultTy <> "," <+> pretty ptrTy <+>
                pretty addr <> alignDoc
            Just (syncScope, memoryOrdering) ->
              "load atomic" <+> optional volatile "volatile" <> pretty resultTy <> "," <+> pretty ptrTy <+>
                pretty addr <+> pretty syncScope <+> pretty memoryOrdering <> alignDoc
    Store volatile addr value atomicity alignment ->
      let ty = typeOf value
          ptrTy = PointerType ty
          alignDoc = if alignment == 0 then mempty else ", align" <+> pretty alignment
       in case atomicity of
            Nothing ->
              "store" <+> optional volatile "volatile" <> pretty ty <+> pretty value <> "," <+>
                pretty ptrTy <+> pretty addr <> alignDoc
            Just (syncScope, memoryOrdering) ->
              "store atomic" <+> optional volatile "volatile" <> pretty ty <+> pretty value <> "," <+>
                pretty ptrTy <+> pretty addr <+> pretty syncScope <+> pretty memoryOrdering <> alignDoc
    Phi cases@((val, _) :| _) ->
      "phi" <+> pretty (typeOf val) <+> (commas $ toList $ fmap prettyPhiCase cases)
      where
        prettyPhiCase (value, name) =
          brackets $ pretty value <> "," <+> pretty name
    Call tcAttr cc fn args ->
      tcDoc <> "call" <+> pretty cc <+> pretty resultType <+> pretty fn <+> prettyArgs
      where
        resultType = case typeOf fn of
          FunctionType retTy _ -> retTy
          _ -> error "Malformed AST, expected function type."
        tcDoc = maybeDoc tcAttr (\tc -> pretty tc <> " ")
        prettyArgs = parens $ commas $ map prettyArg args
        prettyArg arg =
          pretty (typeOf arg) <+> pretty arg
    Ret term ->
      case term of
        Nothing ->
          "ret void"
        Just operand ->
          "ret " <> pretty (typeOf operand) <+> pretty operand
    Br blockName ->
      "br label" <+> "%" <> pretty blockName
    CondBr cond trueLabel falseLabel ->
      "br i1" <+> pretty cond <> ", label" <+> "%" <> pretty trueLabel <> ", label" <+> "%" <> pretty falseLabel
    Switch val defaultLabel cases ->
      "switch" <+> pretty (typeOf val) <+> pretty val <> "," <+> pretty defaultLabel <+>
        list (map prettyCase cases)
      where
        prettyCase (caseVal, label) =
          pretty (typeOf caseVal) <+> pretty caseVal <> ", label" <+> pretty label
    Select c t f ->
      "select" <+> pretty (typeOf c) <+> pretty c <> "," <+>
        pretty (typeOf t) <+> pretty t <> "," <+>
        pretty (typeOf f) <+> pretty f

instance Pretty TailCallAttribute where
  pretty = \case
    Tail -> "tail"
    NoTail -> "notail"
    MustTail -> "musttail"

instance Pretty CallingConvention where
  pretty = \case
    C -> "ccc"
    Fast -> "fastcc"

instance Pretty ComparisonType where
  pretty = \case
    EQ  -> "eq"
    NE -> "ne"
    UGT -> "ugt"
    UGE -> "uge"
    ULT -> "ult"
    ULE -> "ule"
    SGT -> "sgt"
    SGE -> "sge"
    SLT -> "slt"
    SLE -> "sle"

instance Pretty SynchronizationScope where
  pretty = \case
    SingleThread ->
      "syncscope(" <> dquotes "singlethread" <> ")"
    System -> mempty

instance Pretty MemoryOrdering where
  pretty = \case
    Unordered              -> "unordered"
    Monotonic              -> "monotonic"
    Acquire                -> "acquire"
    Release                -> "release"
    AcquireRelease         -> "acq_rel"
    SequentiallyConsistent -> "seq_cst"

prettyArithBinOp :: Doc ann -> Bool -> Bool -> Operand -> Operand -> Doc ann
prettyArithBinOp opName nuw nsw a b =
  opName <+> optional nuw "nuw" <> optional nsw "nsw" <> pretty (typeOf a) <+> pretty a <> "," <+> pretty b

prettyConvertOp :: Doc ann -> Operand -> Type -> Doc ann
prettyConvertOp opName val to =
  opName <+> pretty (typeOf val) <+> pretty val <+> "to" <+> pretty to

optional :: Bool -> Doc ann -> Doc ann
optional b doc = case b of
  False -> mempty
  True -> doc <> " "

maybeDoc :: Maybe a -> (a -> Doc ann) -> Doc ann
maybeDoc = flip (maybe mempty)
