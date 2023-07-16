{-# LANGUAGE RecursiveDo, PolyKinds, RoleAnnotations #-}

module LLVM.Codegen.IRBuilder
  ( IRBuilderT
  , IRBuilder
  , block
  , blockNamed
  , emitBlockStart
  , emitInstr
  , emitInstrVoid
  , emitTerminator
  , BasicBlock(..)
  , runIRBuilderT
  , runIRBuilder
  , MonadIRBuilder(..)

  , add
  , mul
  , sub
  , udiv
  , and
  , or
  , trunc
  , zext
  , ptrtoint
  , bitcast
  , ptrcast
  , icmp
  , alloca
  , gep
  , load
  , store
  , phi
  , call
  , ret
  , retVoid
  , br
  , condBr
  , switch
  , select

  , eq
  , ne
  , sge
  , sgt
  , sle
  , slt
  , uge
  , ugt
  , ule
  , ult
  , if'
  , loop
  , loopWhile
  , loopFor
  , pointerDiff
  , not'
  , Signedness(..)
  , minimum'
  , allocate
  , Path(..), (->>), mkPath
  , addr, deref, assign, update, increment, copy, swap

  , bit
  , int8
  , int16
  , int32
  , int64
  , intN
  , nullPtr
  ) where

import Prelude hiding (EQ, and, or)
import GHC.Stack
import Control.Monad.Fix
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Word
import LLVM.Codegen.Name
import LLVM.Codegen.Operand
import LLVM.Codegen.Type
import LLVM.Codegen.IR
import LLVM.Codegen.IRBuilder.Monad
import LLVM.Codegen.ModuleBuilder


-- Helpers for generating instructions:

add :: (MonadIRBuilder m, HasCallStack) => Operand -> Operand -> m Operand
add lhs rhs =
  emitInstr (typeOf lhs) $ Add Off Off lhs rhs
{-# INLINEABLE add #-}

mul :: (MonadIRBuilder m, HasCallStack) => Operand -> Operand -> m Operand
mul lhs rhs =
  emitInstr (typeOf lhs) $ Mul Off Off lhs rhs
{-# INLINEABLE mul #-}

sub :: (MonadIRBuilder m, HasCallStack) => Operand -> Operand -> m Operand
sub lhs rhs =
  emitInstr (typeOf lhs) $ Sub Off Off lhs rhs
{-# INLINEABLE sub #-}

udiv :: (MonadIRBuilder m, HasCallStack) => Operand -> Operand -> m Operand
udiv lhs rhs =
  emitInstr (typeOf lhs) $ Udiv Off lhs rhs
{-# INLINEABLE udiv #-}

and :: (MonadIRBuilder m, HasCallStack) => Operand -> Operand -> m Operand
and lhs rhs =
  emitInstr (typeOf lhs) $ And lhs rhs
{-# INLINEABLE and #-}

or :: (MonadIRBuilder m, HasCallStack) => Operand -> Operand -> m Operand
or lhs rhs =
  emitInstr (typeOf lhs) $ Or lhs rhs
{-# INLINEABLE or #-}

trunc :: (MonadIRBuilder m, HasCallStack) => Operand -> Type -> m Operand
trunc val ty =
  emitInstr ty $ Trunc val ty
{-# INLINEABLE trunc #-}

zext :: (MonadIRBuilder m, HasCallStack) => Operand -> Type -> m Operand
zext val ty =
  emitInstr ty $ Zext val ty
{-# INLINEABLE zext #-}

ptrtoint :: (MonadIRBuilder m, HasCallStack) => Operand -> Type -> m Operand
ptrtoint val ty =
  emitInstr ty $ PtrToInt val ty
{-# INLINEABLE ptrtoint #-}

-- At the moment not useful because of introduction of opaque pointers.
-- Will become more useful once float or vector types are added again.
bitcast :: (MonadIRBuilder m, HasCallStack) => Operand -> Type -> m Operand
bitcast val ty =
  emitInstr ty $ Bitcast val ty
{-# INLINEABLE bitcast #-}

-- Casts a pointer to be a pointer containing type "ty".
-- This helper function is introduced to smooth the transition between
-- LLVM14 -> LLVM15+ (opaque pointer migration).
-- All bitcasts of pointers should be replaced with ptrcasts
ptrcast :: Type -> Operand -> Operand
ptrcast ty = \case
  LocalRef (PointerType _) name ->
    LocalRef (PointerType ty) name
  ConstantOperand (NullPtr _) ->
    ConstantOperand $ NullPtr ty
  _ ->
    error "'ptrcast' is only supported for pointer operands."
{-# INLINEABLE ptrcast #-}

icmp :: (MonadIRBuilder m, HasCallStack) => ComparisonType -> Operand -> Operand -> m Operand
icmp cmp a b =
  emitInstr i1 $ ICmp cmp a b
{-# INLINEABLE icmp #-}

eq, ne, sge, sgt, sle, slt, uge, ugt, ule, ult
  :: (MonadIRBuilder m, HasCallStack) => Operand -> Operand -> m Operand
eq = icmp EQ
ne = icmp NE
sge = icmp SGE
sgt = icmp SGT
sle = icmp SLE
slt = icmp SLT
uge = icmp UGE
ugt = icmp UGT
ule = icmp ULE
ult = icmp ULT
{-# INLINABLE eq #-}
{-# INLINABLE ne #-}
{-# INLINABLE sge #-}
{-# INLINABLE sgt #-}
{-# INLINABLE sle #-}
{-# INLINABLE slt #-}
{-# INLINABLE uge #-}
{-# INLINABLE ugt #-}
{-# INLINABLE ule #-}
{-# INLINABLE ult #-}

alloca :: (MonadIRBuilder m, HasCallStack) => Type -> Maybe Operand -> Int -> m Operand
alloca ty numElems alignment =
  emitInstr (ptr ty) $ Alloca ty numElems alignment
{-# INLINEABLE alloca #-}

gep :: (HasCallStack, MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> [Operand] -> m Operand
gep operand indices = do
  resultType <- computeGepType (typeOf operand) indices
  case resultType of
    Left err -> error err
    Right ty ->
      emitInstr ty $ GetElementPtr Off operand indices
{-# INLINEABLE gep #-}

computeGepType :: (MonadModuleBuilder m, HasCallStack) => Type -> [Operand] -> m (Either String Type)
computeGepType ty [] = pure $ Right $ PointerType ty
computeGepType (PointerType ty) (_ : idxs) =
  case (ty, null idxs) of
    -- If you want to load something from e.g. i8***, you need to gep + load for each pointer indirection!
    (PointerType{}, False) -> pure $ Left "Opaque pointers support only one gep offset."
    _ -> computeGepType ty idxs
computeGepType (StructureType _ elTys) (ConstantOperand (Int 32 val):is) =
  computeGepType (elTys !! fromIntegral val) is
computeGepType (StructureType _ _) (i:_) =
  pure $ Left $ "Indices into structures should be 32-bit integer constants. (Malformed AST): " <> show i
computeGepType (ArrayType _ elTy) (_:is) = computeGepType elTy is
computeGepType (NamedTypeReference n) is =
  lookupType n >>= \case
    Nothing -> pure $ Left $ "Couldn’t resolve typedef for: " <> show n
    Just ty -> computeGepType ty is
computeGepType ty _ =
  pure $ Left $ "Expecting aggregate type. (Malformed AST): " <> show ty
{-# INLINEABLE computeGepType #-}

load :: (HasCallStack, MonadIRBuilder m) => Operand -> Alignment -> m Operand
load address align =
  case typeOf address of
    PointerType ty ->
      emitInstr ty $ Load Off address Nothing align
    t ->
      error $ "Malformed AST: Expected a pointer type" <> show t
{-# INLINEABLE load #-}

store :: (MonadIRBuilder m, HasCallStack) => Operand -> Alignment -> Operand -> m ()
store address align value =
  emitInstrVoid $ Store Off address value Nothing align
{-# INLINEABLE store #-}

phi :: (HasCallStack, MonadIRBuilder m) => [(Operand, Name)] -> m Operand
phi cases
  | null cases = error "phi instruction should always have > 0 cases!"
  | otherwise =
    let neCases = NE.fromList cases
        ty = typeOf $ fst $ NE.head neCases
     in emitInstr ty $ Phi neCases
{-# INLINEABLE phi #-}

call :: (HasCallStack, MonadIRBuilder m) => Operand -> [Operand] -> m Operand
call fn args = case typeOf fn of
  FunctionType retTy _->
    emitCallInstr retTy
  PointerType (FunctionType retTy _) ->
    emitCallInstr retTy
  _ -> error "Malformed AST, expected function type in 'call' instruction"
  where
    emitCallInstr resultTy =
      if resultTy == VoidType
        then do
          emitInstrVoid $ Call Nothing C fn args
          pure $ ConstantOperand $ Undef void  -- Invalid, but isn't rendered anyway
        else emitInstr resultTy $ Call Nothing C fn args
{-# INLINEABLE call #-}

ret :: (HasCallStack, MonadIRBuilder m) => Operand -> m ()
ret val =
  emitTerminator (Terminator (Ret (Just val)))
{-# INLINEABLE ret #-}

retVoid :: (HasCallStack, MonadIRBuilder m) => m ()
retVoid =
  emitTerminator (Terminator (Ret Nothing))
{-# INLINEABLE retVoid #-}

br :: (MonadIRBuilder m, HasCallStack) => Name -> m ()
br label =
  emitTerminator (Terminator (Br label))
{-# INLINEABLE br #-}

condBr :: (HasCallStack, MonadIRBuilder m) => Operand -> Name -> Name -> m ()
condBr cond trueLabel falseLabel =
  emitTerminator (Terminator (CondBr cond trueLabel falseLabel))
{-# INLINEABLE condBr #-}

switch :: (HasCallStack, MonadIRBuilder m) => Operand -> Name -> [(Operand, Name)] -> m ()
switch value defaultDest dests =
  emitTerminator $ Terminator $ Switch value defaultDest dests
{-# INLINEABLE switch #-}

select :: (HasCallStack, MonadIRBuilder m) => Operand -> Operand -> Operand -> m Operand
select c t f =
  emitInstr (typeOf t) $ Select c t f
{-# INLINEABLE select #-}

if' :: (HasCallStack, MonadIRBuilder m, MonadFix m)
    => Operand -> m a -> m ()
if' condition asm = mdo
  condBr condition ifBlock end
  ifBlock <- blockNamed "if"
  _ <- asm
  br end
  end <- blockNamed "end_if"
  pure ()
{-# INLINEABLE if' #-}

loop :: (HasCallStack, MonadIRBuilder m, MonadFix m) => m a -> m ()
loop asm = mdo
  br begin
  begin <- blockNamed "loop"
  _ <- asm
  br begin
{-# INLINEABLE loop #-}

loopWhile :: (HasCallStack, MonadIRBuilder m, MonadFix m)
          => m Operand -> m a -> m ()
loopWhile condition asm = mdo
  br begin
  begin <- blockNamed "while_begin"
  result <- condition
  condBr result body end
  body <- blockNamed "while_body"
  _ <- asm
  br begin
  end <- blockNamed "while_end"
  pure ()
{-# INLINEABLE loopWhile #-}

loopFor :: (HasCallStack, MonadModuleBuilder m, MonadIRBuilder m, MonadFix m)
        => Operand
        -> (Operand -> m Operand)
        -> (Operand -> m Operand)
        -> (Operand -> m a)
        -> m ()
loopFor beginValue condition post asm = mdo
  start <- currentBlock
  br begin
  begin <- blockNamed "for_begin"
  loopValue <- phi [(beginValue, start), (updatedValue, bodyEnd)]
  result <- condition loopValue
  condBr result bodyStart end
  bodyStart <- blockNamed "for_body"
  _ <- asm loopValue
  updatedValue <- post loopValue
  bodyEnd <- currentBlock
  br begin
  end <- blockNamed "for_end"
  pure ()
{-# INLINEABLE loopFor #-}

-- NOTE: diff is in bytes! (Different compared to C and C++)
pointerDiff :: (HasCallStack, MonadIRBuilder m)
            => Type -> Operand -> Operand -> m Operand
pointerDiff ty a b = do
  a' <- ptrtoint a i64
  b' <- ptrtoint b i64
  result <- sub a' b'
  if ty == i64
    then pure result
    else trunc result ty
{-# INLINEABLE pointerDiff #-}

-- | Calculates the logical not of a boolean 'Operand'.
--   NOTE: This assumes the 'Operand' is of type 'i1', this is not checked!
--   Passing in an argument of another width will lead to a crash in LLVM.
not' :: (HasCallStack, MonadIRBuilder m)
     => Operand -> m Operand
not' bool =
  select bool (bit 0) (bit 1)
{-# INLINEABLE not' #-}

data Signedness = Signed | Unsigned

--   NOTE: No check is made if the 2 operands have the same 'Type'!
minimum' :: (HasCallStack, MonadIRBuilder m)
         => Signedness -> Operand -> Operand -> m Operand
minimum' sign a b = do
  let inst = case sign of
        Signed -> slt
        Unsigned -> ult
  isLessThan <- inst a b
  select isLessThan a b
{-# INLINEABLE minimum' #-}

allocate :: (HasCallStack, MonadIRBuilder m) => Type -> Operand -> m Operand
allocate ty beginValue = do
  value <- alloca ty Nothing 0
  store value 0 beginValue
  pure value
{-# INLINEABLE allocate #-}

newtype Path (a :: k) (b :: k)
  = Path (NonEmpty Operand)
  deriving (Eq, Show)
type role Path nominal nominal

mkPath :: [Operand] -> Path a b
mkPath path = Path (int32 0 :| path)

(->>) :: Path a b -> Path b c -> Path a c
Path a2b ->> Path b2c =
  let b2c' = if NE.head b2c == int32 0
               then NE.tail b2c
               else NE.toList b2c
   in Path $ NE.head a2b :| (NE.tail a2b ++ b2c')

addr :: (MonadModuleBuilder m, MonadIRBuilder m, HasCallStack)
     => Path a b -> Operand -> m Operand
addr path p = gep p (pathToIndices path)
  where
    pathToIndices :: Path a b -> [Operand]
    pathToIndices (Path indices) =
      NE.toList indices
{-# INLINEABLE addr #-}

deref :: (MonadModuleBuilder m, MonadIRBuilder m, HasCallStack)
      => Path a b -> Operand -> m Operand
deref path p = do
  address <- addr path p
  load address 0
{-# INLINEABLE deref #-}

assign :: (MonadModuleBuilder m, MonadIRBuilder m, HasCallStack)
       => Path a b -> Operand -> Operand -> m ()
assign path p value = do
  dstAddr <- addr path p
  store dstAddr 0 value
{-# INLINEABLE assign #-}

update :: (MonadModuleBuilder m, MonadIRBuilder m, HasCallStack)
       => Path a b
       -> Operand
       -> (Operand -> m Operand)
       -> m ()
update path p f = do
  dstAddr <- addr path p
  store dstAddr 0 =<< f =<< load dstAddr 0
{-# INLINEABLE update #-}

increment :: (MonadModuleBuilder m, MonadIRBuilder m, HasCallStack)
          => (Integer -> Operand) -> Path a b -> Operand -> m ()
increment ty path p =
  update path p (add (ty 1))
{-# INLINEABLE increment #-}

copy :: (MonadModuleBuilder m, MonadIRBuilder m, HasCallStack)
     => Path a b -> Operand -> Operand -> m ()
copy path src dst = do
  value <- deref path src
  assign path dst value
{-# INLINEABLE copy #-}

swap :: (MonadModuleBuilder m, MonadIRBuilder m, HasCallStack)
     => Path a b -> Operand -> Operand -> m ()
swap path lhs rhs = do
  tmp <- deref path lhs
  copy path rhs lhs
  assign path rhs tmp
{-# INLINEABLE swap #-}


bit :: Integer -> Operand
bit b =
  intN 1 $ if b == 0 then 0 else 1

int8 :: Integer -> Operand
int8 =
  intN 8

int16 :: Integer -> Operand
int16 =
  intN 16

int32 :: Integer -> Operand
int32 =
  intN 32

int64 :: Integer -> Operand
int64 =
  intN 64

intN :: Word32 -> Integer -> Operand
intN bits value =
  ConstantOperand $ Int bits value

nullPtr :: Type -> Operand
nullPtr =
  ConstantOperand . NullPtr
