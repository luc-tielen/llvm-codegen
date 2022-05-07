{-# LANGUAGE RecursiveDo, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults#-}

module LLVM.Codegen
  ( module LLVM.Codegen  -- TODO clean up exports
  ) where

import Control.Monad.State
import Control.Monad.Reader
import Data.Functor.Identity
import qualified Data.DList as DList
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.DList (DList)
import Data.Foldable
import Data.Monoid
import Data.Maybe
import LLVM.NameSupply


data Type
  = IntType Int
  | FunctionType Type [Type]
  | PointerType Type
  deriving Show

data Operand
  = LocalRef Type Name
  | GlobalRef Type Name
  deriving Show

data IR
  = Add Operand Operand
  | Ret (Maybe Operand)
  deriving Show

newtype Terminator
  = Terminator IR
  deriving Show

data BasicBlock
  = BB
  { bbName :: Name
  , bbInstructions :: DList (Operand, IR)
  , bbTerminator :: Terminator
  } deriving Show

data PartialBlock
  = PartialBlock
  { pbName :: Name
  , pbInstructions :: DList (Operand, IR)
  , pbTerminator :: First Terminator
  }

data IRBuilderState
  = IRBuilderState
  { basicBlocks :: DList BasicBlock
  , currentBlock :: PartialBlock
  }

newtype IRBuilderT m a
  = IRBuilderT (StateT IRBuilderState (NameSupplyT m) a)
  deriving ( Functor, Applicative, Monad, MonadFix
           , MonadReader (Maybe Name), MonadState IRBuilderState
           , MonadNameSupply
           )
  via StateT IRBuilderState (NameSupplyT m)

type IRBuilder = IRBuilderT Identity

runIRBuilderT :: Monad m => IRBuilderT m a -> m [BasicBlock]
runIRBuilderT (IRBuilderT m) = do
  let partialBlock = PartialBlock (Name "start") mempty mempty
      result = runNameSupplyT $ execStateT m (IRBuilderState mempty partialBlock)
  previousBlocks <- fmap (flip DList.apply mempty . basicBlocks) result
  currentBlk <- fmap currentBlock result
  pure $ previousBlocks ++ [partialBlockToBasicBlock currentBlk]

runIRBuilder :: IRBuilder a -> [BasicBlock]
runIRBuilder = runIdentity . runIRBuilderT

block :: Monad m => IRBuilderT m Name
block = do
  blockName <- ask >>= \case
    Nothing -> do
      name <- fresh
      pure $ Name $ "block_" <> unName name
    Just _sugg ->
      fresh

  modify $ \s ->
    let currBlock = currentBlock s
        blocks = basicBlocks s
     in s { basicBlocks = DList.snoc blocks (partialBlockToBasicBlock currBlock)
          , currentBlock = PartialBlock blockName mempty mempty
          }
  pure blockName

partialBlockToBasicBlock :: PartialBlock -> BasicBlock
partialBlockToBasicBlock pb =
  let currentTerm = fromMaybe (Terminator $ Ret Nothing) $ getFirst $ pbTerminator pb
  in BB (pbName pb) (pbInstructions pb) currentTerm

emitTerminator :: Monad m => Terminator -> IRBuilderT m ()
emitTerminator term =
  modify $ \s ->
    s { currentBlock = (currentBlock s) { pbTerminator = First (Just term) <> pbTerminator (currentBlock s) } }

-- NOTE: Only used internally, this creates an unassigned operand
mkOperand :: Monad m => Type -> IRBuilderT m Operand
mkOperand ty = do
  name <- fresh
  pure $ LocalRef ty name

emitInstr :: Monad m => Type -> IR -> IRBuilderT m Operand
emitInstr ty instr = do
  operand <- mkOperand ty
  modify (addInstrToCurrentBlock operand)
  pure operand
  where
    addInstrToCurrentBlock operand s =
      -- TODO: record dot syntax? or create a helper function if this pattern occurs a lot..
      let instrs = DList.snoc (pbInstructions . currentBlock $ s) (operand, instr)
       in s { currentBlock = (currentBlock s) { pbInstructions = instrs } }


data Definition = Function Name Type [Type] [BasicBlock]
  deriving Show

type ModuleBuilderState = [Definition]

newtype ModuleBuilderT m a
  = ModuleBuilder (StateT ModuleBuilderState m a)
  deriving (Functor, Applicative, Monad, MonadState ModuleBuilderState, MonadFix)
  via StateT ModuleBuilderState m

type ModuleBuilder = ModuleBuilderT Identity

runModuleBuilderT :: Monad m => ModuleBuilderT m a -> m [Definition]
runModuleBuilderT (ModuleBuilder m) =
  execStateT m []

runModuleBuilder :: ModuleBuilder a -> [Definition]
runModuleBuilder = runIdentity . runModuleBuilderT

typeOf :: Operand -> Type
typeOf = \case
  LocalRef ty _ -> ty
  GlobalRef ty _ -> ty

add :: Monad m => Operand -> Operand -> IRBuilderT m Operand
add lhs rhs =
  emitInstr (typeOf lhs) $ Add lhs rhs

ret :: Monad m => Operand -> IRBuilderT m ()
ret val =
  emitTerminator (Terminator (Ret (Just val)))

function :: Monad m => Name -> [Type] -> Type -> ([Operand] -> IRBuilderT (ModuleBuilderT m) a) -> ModuleBuilderT m Operand
function name tys retTy fnBody = do
  instrs <- runIRBuilderT $ do
    operands <- traverse mkOperand tys
    fnBody operands
  modify $ (Function name retTy tys instrs:)
  pure $ GlobalRef (PointerType (FunctionType retTy tys)) name


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
