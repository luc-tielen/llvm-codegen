module LLVM.Codegen.IR
  ( IR(..)
  , Terminator(..)
  , ComparisonType(..)
  , CallingConvention(..)
  , TailCallAttribute(..)
  , SynchronizationScope(..)
  , MemoryOrdering(..)
  , Alignment
  , Flag(..)
  , NUW
  , NSW
  , Exact
  , Inbounds
  , Volatile
  , renderIR
  ) where

import Prelude hiding (EQ)
import LLVM.Codegen.Name
import LLVM.Codegen.Operand
import LLVM.Codegen.Type
import LLVM.Codegen.Flag
import LLVM.Pretty
import Data.Word
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE


data NUW
data NSW
data Exact
data Inbounds
data Volatile

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
  = Tail
  | MustTail
  | NoTail
  deriving Show

data CallingConvention
  = C
  | Fast
  -- TODO add others as needed
  deriving Show

data IR
  = Add !(Flag NUW) !(Flag NSW) !Operand !Operand
  | Mul !(Flag NUW) !(Flag NSW) !Operand !Operand
  | Sub !(Flag NUW) !(Flag NSW) !Operand !Operand
  | Udiv !(Flag Exact) !Operand !Operand
  | And !Operand !Operand
  | Or !Operand !Operand
  | Trunc !Operand !Type
  | Zext !Operand !Type
  | Bitcast !Operand !Type
  | ICmp !ComparisonType !Operand !Operand
  | PtrToInt !Operand !Type
  | Alloca !Type !(Maybe Operand) !Int
  | GetElementPtr !(Flag Inbounds) !Operand ![Operand]
  | Load !(Flag Volatile) !Operand !(Maybe Atomicity) !Alignment
  | Store !(Flag Volatile) !Operand !Operand !(Maybe Atomicity) !Alignment
  | Phi !(NonEmpty (Operand, Name))
  | Call !(Maybe TailCallAttribute) !CallingConvention !Operand ![Operand]  -- TODO support param attributes
  -- Terminators
  | Ret !(Maybe Operand)
  | Br !Name
  | CondBr !Operand !Name !Name
  | Switch !Operand !Name ![(Operand, Name)]
  | Select !Operand !Operand !Operand
  deriving Show

newtype Terminator
  = Terminator IR
  deriving Show

renderTCA :: Renderer TailCallAttribute
renderTCA buf = \case
  Tail -> buf |># "tail"#
  NoTail -> buf |># "notail"#
  MustTail -> buf |># "musttail"#
{-# INLINABLE renderTCA #-}

renderCC :: Renderer CallingConvention
renderCC buf = \case
  C -> buf |># "ccc"#
  Fast -> buf |># "fastcc"#
{-# INLINABLE renderCC #-}

renderComparisonType :: Renderer ComparisonType
renderComparisonType buf = \case
  EQ  -> buf |># "eq"#
  NE  -> buf |># "ne"#
  UGT -> buf |># "ugt"#
  UGE -> buf |># "uge"#
  ULT -> buf |># "ult"#
  ULE -> buf |># "ule"#
  SGT -> buf |># "sgt"#
  SGE -> buf |># "sge"#
  SLT -> buf |># "slt"#
  SLE -> buf |># "sle"#
{-# INLINABLE renderComparisonType #-}

renderMemoryOrdering :: Renderer MemoryOrdering
renderMemoryOrdering buf = \case
  Unordered              -> buf |># "unordered"#
  Monotonic              -> buf |># "monotonic"#
  Acquire                -> buf |># "acquire"#
  Release                -> buf |># "release"#
  AcquireRelease         -> buf |># "acq_rel"#
  SequentiallyConsistent -> buf |># "seq_cst"#
{-# INLINABLE renderMemoryOrdering #-}

renderSyncScope :: Renderer SynchronizationScope
renderSyncScope buf = \case
  SingleThread ->
    buf |># "syncscope(\"singlethread\")"#
  System ->
    buf
{-# INLINABLE renderSyncScope #-}

renderIR :: Renderer IR
renderIR buf = \case
  Add nuw nsw a b ->
    renderArithBinOp buf "add "# nuw nsw a b
  Mul nuw nsw a b ->
    renderArithBinOp buf "mul "# nuw nsw a b
  Sub nuw nsw a b ->
    renderArithBinOp buf "sub "# nuw nsw a b
  Udiv exact a b ->
    ((((optional exact (buf |># "udiv "#) (|># "exact "#) `renderType` typeOf a)
    |>. ' ') `renderOperand` a) |># ", "#) `renderOperand` b
  And a b ->
    ((((buf |># "and "#) `renderType` typeOf a) |>. ' ') `renderOperand` a |># ", "#) `renderOperand` b
  Or a b ->
    ((((buf |># "or "#) `renderType` typeOf a) |>. ' ') `renderOperand` a |># ", "#) `renderOperand` b
  ICmp cmp a b ->
    (((((buf |># "icmp "#) `renderComparisonType` cmp |>. ' ') `renderType` typeOf a) |>. ' ') `renderOperand` a |># ", "#) `renderOperand` b
  Trunc val to ->
    renderConvertOp buf "trunc "# val to
  Zext val to ->
    renderConvertOp buf "zext "# val to
  Bitcast val to ->
    renderConvertOp buf "bitcast "# val to
  PtrToInt val to ->
    renderConvertOp buf "ptrtoint "# val to
  Alloca ty mNumElems alignment ->
    renderMaybe
      (renderMaybe ((buf |># "alloca "#) `renderType` ty) mNumElems
        (\buf' count -> ((buf' |># ", "#) `renderType` typeOf count |>. ' ') `renderOperand` count))
      (if alignment == 0 then Nothing else Just alignment)
      (\buf' align -> buf' |># ", align "# |>$ align)
  GetElementPtr inbounds pointer indices ->
    case typeOf pointer of
      ty@(PointerType innerTy) ->
        commas (((optional inbounds (buf |># "getelementptr "#) (|># "inbounds "#) `renderType` innerTy |># ", "#) `renderType` ty |>. ' ')
        `renderOperand` pointer |># ", "#) indices prettyIndex
      _ ->
        buf |> error "Operand given to `getelementptr` that is not a pointer!"
    where
      prettyIndex :: Buffer %1 -> Operand -> Buffer
      prettyIndex buf' i = (renderType buf' (typeOf i) |>. ' ') `renderOperand` i
  Load volatile addr atomicity alignment ->
    case atomicity of
      Nothing ->
        withAlignment alignment
          ((((optional volatile (buf |># "load "#) (|># "volatile "#)
            `renderType` resultTy) |># ", "#) `renderType` ptrTy |>. ' ') `renderOperand` addr)
      Just (syncScope, memoryOrdering) ->
        withAlignment alignment
          ((((((optional volatile (buf |># "load atomic "#) (|># "volatile "#)
            `renderType` resultTy) |># ", "#) `renderType` ptrTy |>. ' ') `renderOperand` addr |>. ' ')
            `renderSyncScope` syncScope |>. ' ') `renderMemoryOrdering` memoryOrdering)
    where
      ptrTy = typeOf addr
      resultTy = case ptrTy of
        PointerType ty -> ty
        _ -> error "Malformed AST, expected pointer type."
  Store volatile addr value atomicity alignment ->
    case atomicity of
      Nothing ->
        withAlignment alignment
          ((((optional volatile (buf |># "store "#) (|># "volatile "#) `renderType` ty |>. ' ') `renderOperand` value |># ", "#)
            `renderType` ptrTy |>. ' ') `renderOperand` addr)
      Just (syncScope, memoryOrdering) ->
        withAlignment alignment
          ((((((optional volatile (buf |># "store atomic "#) (|># "volatile "#) `renderType` ty |>. ' ') `renderOperand` value |># ", "#)
            `renderType` ptrTy |>. ' ') `renderOperand` addr |>. ' ') `renderSyncScope` syncScope |>. ' ') `renderMemoryOrdering` memoryOrdering)
    where
      ty = typeOf value
      ptrTy = PointerType ty
  Phi cases@((val, _) :| _) ->
    commas ((buf |># "phi "#) `renderType` typeOf val |>. ' ') (NE.toList cases) renderPhiCase
    where
      renderPhiCase :: Renderer (Operand, Name)
      renderPhiCase buf' (value, name) =
        brackets buf' (\buf'' -> (renderOperand buf'' value |># ", %"#) `renderName` name)
  Call tcAttr cc fn args ->
    (((renderMaybe buf tcAttr (\buf' tca -> renderTCA buf' tca |>. ' ')
      |># "call "#) `renderCC` cc |>. ' ') `renderType` resultType |>. ' ')
      `renderOperand` fn `renderArgs` args
    where
      resultType = case typeOf fn of
        PointerType (FunctionType retTy _) -> retTy
        FunctionType retTy _ -> retTy
        _ -> error "Malformed AST, expected function type."
      renderArgs :: Renderer [Operand]
      renderArgs buf' args' = tupled buf' args' renderArg
      renderArg :: Renderer Operand
      renderArg buf' arg =
        (renderType buf' (typeOf arg) |>. ' ') `renderOperand` arg
  Ret term -> case term of
    Nothing ->
      buf |># "ret void"#
    Just operand ->
      ((buf |># "ret "#) `renderType` typeOf operand |>. ' ') `renderOperand` operand
  Br blockName ->
    (buf |># "br label %"#) `renderName` blockName
  CondBr cond trueLabel falseLabel ->
    (((buf |># "br i1 "#) `renderOperand` cond
      |># ", label %"#) `renderName` trueLabel
      |># ", label %"#) `renderName` falseLabel
  Switch val defaultLabel cases ->
    brackets ((((buf |># "switch "#) `renderType` typeOf val |>. ' ') `renderOperand` val |># ", label %"#) `renderName` defaultLabel |>. ' ')
      (\buf' -> hsep buf' cases renderCase)
    where
      renderCase :: Renderer (Operand, Name)
      renderCase buf' (caseVal, label) =
        ((renderType buf' (typeOf caseVal) |>. ' ') `renderOperand` caseVal |># ", label %"#) `renderName` label
  Select c t f ->
    ((((((buf |># "select "#) `renderType` typeOf c |>. ' ') `renderOperand` c |># ", "#)
      `renderType` typeOf t |>. ' ') `renderOperand` t |># ", "#)
      `renderType` typeOf f |>. ' ') `renderOperand` f
  where
    withAlignment :: Word32 -> Buffer %1 -> Buffer
    withAlignment alignment buf' =
      if alignment == 0
        then buf'
        else buf' |># ", align "# |>$ alignment
{-# INLINABLE renderIR #-}

renderArithBinOp :: Buffer %1 -> Addr# -> Flag NUW -> Flag NSW -> Operand -> Operand -> Buffer
renderArithBinOp buf opName nuw nsw a b =
  (((optional nsw (optional nuw
    (buf |># opName) (|># "nuw "#)) (|># "nsw "#) `renderType` typeOf a) |>. ' ') `renderOperand` a |># ", "#) `renderOperand` b
{-# INLINABLE renderArithBinOp #-}

renderConvertOp :: Buffer %1 -> Addr# -> Operand -> Type -> Buffer
renderConvertOp buf opName val to =
  ((((buf |># opName) `renderType` typeOf val) |>. ' ') `renderOperand` val |># " to "#) `renderType` to
{-# INLINABLE renderConvertOp #-}
