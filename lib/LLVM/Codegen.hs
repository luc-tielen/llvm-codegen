{-# LANGUAGE RecursiveDo, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults#-}

module LLVM.Codegen
  ( module LLVM.Codegen  -- TODO clean up exports
  ) where

import Control.Monad.State.Lazy
import Data.Functor.Identity
import Data.Foldable
import qualified Data.Text as T
import qualified Data.List as L
import Data.Text (Text)

newtype Operand
  = Operand { unOperand :: Text }
  deriving Show  -- TODO remove

newtype Label = Label Text
  deriving Show

data IR
  = Add Operand Operand
  deriving Show  -- TODO remove


type Counter = Int

-- TODO: store first terminator
data IRBuilderState
  = IRBuilderState
  { operandCounter :: Counter
  , instructions :: [(Operand, IR)]  -- TODO diff list
  }

newtype IRBuilderT m a
  = IRBuilderT (StateT IRBuilderState m a)
  deriving (Functor, Applicative, Monad, MonadState IRBuilderState, MonadFix)
  via StateT IRBuilderState m

type IRBuilder = IRBuilderT Identity

runIRBuilderT :: Monad m => IRBuilderT m a -> m [(Operand, IR)]
runIRBuilderT (IRBuilderT m) =
  -- TODO: need scc?
  fmap (reverse . instructions) <$> execStateT m $ IRBuilderState 0 mempty

runIRBuilder :: IRBuilder a -> [(Operand, IR)]
runIRBuilder = runIdentity . runIRBuilderT


-- TODO: rename Label -> Name?
data Definition = Function Label [Type] [(Operand, IR)]  -- TODO: introduce basic blocks
  deriving Show

type ModuleBuilderState = [Definition]

-- TODO transformer
newtype ModuleBuilder a
  = ModuleBuilder (State ModuleBuilderState a)
  deriving (Functor, Applicative, Monad, MonadState ModuleBuilderState, MonadFix)
  via State ModuleBuilderState

runModuleBuilder :: ModuleBuilder a -> [Definition]
runModuleBuilder (ModuleBuilder m) =
  execState m []


add :: Monad m => Operand -> Operand -> IRBuilderT m Operand
add lhs rhs = emitInstr $ Add lhs rhs

emitInstr :: Monad m => IR -> IRBuilderT m Operand
emitInstr instr = do
  operand <- mkOperand
  modify $ \s -> s { instructions = (operand, instr) : instructions s }
  pure operand

-- NOTE: Only used internally, this creates an unassigned operand
mkOperand :: Monad m => IRBuilderT m Operand
mkOperand = do
  count <- gets operandCounter
  modify $ \s -> s { operandCounter = operandCounter s + 1 }
  pure $ Operand $ "%" <> T.pack (show count)

data Type = IntType Int
  deriving Show  -- TODO remove

function :: Label -> [Type] -> ([Operand] -> IRBuilderT ModuleBuilder a) -> ModuleBuilder Operand
function lbl@(Label name) tys fnBody = do
  instrs <- runIRBuilderT $ do
    operands <- traverse (const mkOperand) tys
    fnBody operands
  let fn = Function lbl tys instrs
  modify $ (fn:)  -- TODO: store operand
  pure $ Operand $ "@" <> name

exampleIR :: [(Operand, IR)]
exampleIR = runIRBuilder $ mdo
  let a = Operand "a"
  let b = Operand "b"
  d <- add a c
  c <- add a b
  pure d

exampleModule :: [Definition]
exampleModule = runModuleBuilder $ do
  function (Label "add") [IntType 1, IntType 32] $ \[x, y] -> mdo
    add x y

pp :: [Definition] -> Text
pp defs =
  mconcat $ L.intersperse "\n" $ map ppDefinition defs

ppDefinition :: Definition -> Text
ppDefinition = \case
  Function (Label name) argTys body ->
    "define ccc void @" <> name <> "(" <> fold (L.intersperse ", " (zipWith ppArg [0..] argTys)) <> ")" <> "{\n" <>
      ppBody body <> "\n" <>
      "}"
    where
      ppArg i _argTy = ppOperand $ Operand $ "%" <> T.pack (show i)
      ppBody instrs = fold $ L.intersperse "\n" $ map (uncurry ppStmt) instrs
      ppStmt :: Operand -> IR -> Text
      ppStmt operand instr = ppOperand operand <> " = " <> ppInstr instr
      ppInstr :: IR -> Text
      ppInstr = \case
        Add a b -> "add " <> ppOperand a <> " " <> ppOperand b

ppOperand :: Operand -> Text
ppOperand = unOperand

-- $> putStrLn . Data.Text.unpack $ pp exampleModule
