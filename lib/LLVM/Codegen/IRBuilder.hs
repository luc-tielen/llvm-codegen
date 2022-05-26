{-# LANGUAGE RecursiveDo #-}

module LLVM.Codegen.IRBuilder
  ( IRBuilderT
  , IRBuilder
  , block
  , named
  , emitBlockStart
  , emitInstr
  , emitInstrVoid
  , emitTerminator
  , BasicBlock(..)
  , runIRBuilderT
  , runIRBuilder
  , MonadIRBuilder

  , add
  , mul
  , sub
  , udiv
  , and
  , trunc
  , zext
  , ptrtoint
  , bitcast
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

  , if'
  , loop
  , loopWhile
  , loopFor

  , bit
  , int8
  , int16
  , int32
  , int64
  , intN
  ) where

import Prelude hiding (and)
import GHC.Stack
import Control.Monad.Fix
import qualified Data.List.NonEmpty as NE
import Data.Word
import LLVM.Codegen.NameSupply
import LLVM.Codegen.Operand
import LLVM.Codegen.Type
import LLVM.Codegen.IR
import LLVM.Codegen.IRBuilder.Monad
import LLVM.Codegen.ModuleBuilder


-- Helpers for generating instructions:

add :: (MonadNameSupply m, MonadIRBuilder m) => Operand -> Operand -> m Operand
add lhs rhs =
  emitInstr (typeOf lhs) $ Add Off Off lhs rhs

mul :: (MonadNameSupply m, MonadIRBuilder m) => Operand -> Operand -> m Operand
mul lhs rhs =
  emitInstr (typeOf lhs) $ Mul Off Off lhs rhs

sub :: (MonadNameSupply m, MonadIRBuilder m) => Operand -> Operand -> m Operand
sub lhs rhs =
  emitInstr (typeOf lhs) $ Sub Off Off lhs rhs

udiv :: (MonadNameSupply m, MonadIRBuilder m) => Operand -> Operand -> m Operand
udiv lhs rhs =
  emitInstr (typeOf lhs) $ Udiv Off lhs rhs

and :: (MonadNameSupply m, MonadIRBuilder m) => Operand -> Operand -> m Operand
and lhs rhs =
  emitInstr (typeOf lhs) $ And lhs rhs

trunc :: (MonadNameSupply m, MonadIRBuilder m) => Operand -> Type -> m Operand
trunc val ty =
  emitInstr ty $ Trunc val ty

zext :: (MonadNameSupply m, MonadIRBuilder m) => Operand -> Type -> m Operand
zext val ty =
  emitInstr ty $ Zext val ty

ptrtoint :: (MonadNameSupply m, MonadIRBuilder m) => Operand -> Type -> m Operand
ptrtoint val ty =
  emitInstr ty $ PtrToInt val ty

bitcast :: (MonadNameSupply m, MonadIRBuilder m) => Operand -> Type -> m Operand
bitcast val ty =
  emitInstr ty $ Bitcast val ty

icmp :: (MonadNameSupply m, MonadIRBuilder m) => ComparisonType -> Operand -> Operand -> m Operand
icmp cmp a b =
  emitInstr i1 $ ICmp cmp a b

alloca :: (MonadNameSupply m, MonadIRBuilder m) => Type -> (Maybe Operand) -> Int -> m Operand
alloca ty numElems alignment =
  emitInstr ty $ Alloca ty numElems alignment

gep :: (HasCallStack, MonadNameSupply m, MonadModuleBuilder m, MonadIRBuilder m)
    => Operand -> [Operand] -> m Operand
gep operand indices = do
  resultType <- computeGepType (typeOf operand) indices
  case resultType of
    Left err -> error err -- TODO
    Right ty ->
      emitInstr ty $ GetElementPtr Off operand indices

computeGepType :: (MonadModuleBuilder m, HasCallStack) => Type -> [Operand] -> m (Either String Type)
computeGepType ty [] = pure $ Right $ PointerType ty
computeGepType (PointerType ty) (_ : is) = computeGepType ty is
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

load :: (HasCallStack, MonadNameSupply m, MonadIRBuilder m) => Operand -> Alignment -> m Operand
load addr align =
  case typeOf addr of
    PointerType ty ->
      emitInstr ty $ Load Off addr Nothing align
    _ ->
      error "Malformed AST: Expected a pointer type"

store :: MonadIRBuilder m => Operand -> Alignment -> Operand -> m ()
store addr align value =
  emitInstrVoid $ Store Off addr value Nothing align

phi :: (HasCallStack, MonadNameSupply m, MonadIRBuilder m) => [(Operand, Name)] -> m Operand
phi cases
  | null cases = error "phi instruction should always have > 0 cases!"
  | otherwise =
    let neCases = NE.fromList cases
        ty = typeOf $ fst $ NE.head neCases
     in emitInstr ty $ Phi neCases

call :: (HasCallStack, MonadNameSupply m, MonadIRBuilder m) => Operand -> [Operand] -> m Operand
call fn args = case typeOf fn of
  FunctionType retTy _->
    emitCallInstr retTy
  PointerType (FunctionType retTy _) ->
    emitCallInstr retTy
  _ -> error "Malformed AST, expected function type in 'call' instruction"
  where
    emitCallInstr resultTy =
      emitInstr resultTy $ Call Nothing C fn args

ret :: MonadIRBuilder m => Operand -> m ()
ret val =
  emitTerminator (Terminator (Ret (Just val)))

retVoid :: MonadIRBuilder m => m ()
retVoid =
  emitTerminator (Terminator (Ret Nothing))

br :: MonadIRBuilder m => Name -> m ()
br label =
  emitTerminator (Terminator (Br label))

condBr :: MonadIRBuilder m => Operand -> Name -> Name -> m ()
condBr cond trueLabel falseLabel =
  emitTerminator (Terminator (CondBr cond trueLabel falseLabel))

switch :: MonadIRBuilder m => Operand -> Name -> [(Operand, Name)] -> m ()
switch value defaultDest dests =
  emitTerminator $ Terminator $ Switch value defaultDest dests

select :: (MonadNameSupply m, MonadIRBuilder m) => Operand -> Operand -> Operand -> m Operand
select c t f =
  emitInstr (typeOf t) $ Select c t f

if' :: (MonadNameSupply m, MonadIRBuilder m, MonadFix m)
    => Operand -> m a -> m ()
if' condition asm = mdo
  condBr condition ifBlock end
  ifBlock <- block `named` "if"
  _ <- asm
  br end
  end <- block `named` "end_if"
  pure ()

loop :: (MonadNameSupply m, MonadIRBuilder m, MonadFix m) => m a -> m ()
loop asm = mdo
  br begin
  begin <- block `named` "loop"
  _ <- asm
  br begin

loopWhile :: (MonadNameSupply m, MonadIRBuilder m, MonadFix m)
          => m Operand -> m a -> m ()
loopWhile condition asm = mdo
  br begin
  begin <- block `named` "while_begin"
  result <- condition
  condBr result body end
  body <- block `named` "while_body"
  _ <- asm
  br begin
  end <- block `named` "while_end"
  pure ()

loopFor :: (MonadNameSupply m, MonadModuleBuilder m, MonadIRBuilder m, MonadFix m)
        => Operand
        -> (Operand -> m Operand)
        -> (Operand -> m Operand)
        -> (Operand -> m a)
        -> m ()
loopFor beginValue condition post asm = mdo
  start <- currentBlock
  br begin
  begin <- block `named` "for_begin"
  loopValue <- phi [(beginValue, start), (updatedValue, bodyEnd)]
  result <- condition loopValue
  condBr result bodyStart end
  bodyStart <- block `named` "for_body"
  _ <- asm loopValue
  updatedValue <- post loopValue
  bodyEnd <- currentBlock
  br begin
  end <- block `named` "for_end"
  pure ()

bit :: Bool -> Operand
bit b =
  intN 1 $ if b then 1 else 0

int8 :: Int -> Operand
int8 =
  intN 8 . toInteger

int16 :: Int -> Operand
int16 =
  intN 16 . toInteger

int32 :: Int -> Operand
int32 =
  intN 32 . toInteger

int64 :: Int -> Operand
int64 =
  intN 64 . toInteger

intN :: Word32 -> Integer -> Operand
intN bits value =
  ConstantOperand $ Int bits value
