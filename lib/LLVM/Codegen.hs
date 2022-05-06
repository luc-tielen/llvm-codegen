{-# LANGUAGE RecursiveDo, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults#-}

module LLVM.Codegen
  ( module LLVM.Codegen  -- TODO clean up exports
  ) where

import Control.Monad.State.Lazy
import Control.Monad.RWS.Lazy
import Data.Functor.Identity
import Data.Foldable
import Data.Maybe
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Map as M
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.DList as DList
import Data.DList (DList)
import Data.String


data Operand
  = LocalRef { unOperand :: Name }
  | GlobalRef { unOperand :: Name }
  deriving Show

newtype Name = Name { unName :: Text }
  deriving (Eq, Ord, Show)

instance IsString Name where
  fromString = Name . fromString


data IR
  = Add Operand Operand
  | Ret (Maybe Operand)
  deriving Show


type Counter = Int

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

newtype Terminator
  = Terminator IR
  deriving Show

data IRBuilderState
  = IRBuilderState
  { operandCounter :: Counter
  , blockCounter :: Counter
  , nameMap :: Map Name Int
  , basicBlocks :: DList BasicBlock
  , currentBlock :: PartialBlock
  }

newtype IRBuilderT m a
  = IRBuilderT (RWST (Maybe Name) () IRBuilderState m a)
  deriving (Functor, Applicative, Monad, MonadReader (Maybe Name), MonadState IRBuilderState, MonadFix)
  via RWST (Maybe Name) () IRBuilderState m

type IRBuilder = IRBuilderT Identity

runIRBuilderT :: Monad m => IRBuilderT m a -> m [BasicBlock]
runIRBuilderT (IRBuilderT m) = do
  let partialBlock = PartialBlock (Name "start") mempty mempty
      result = fst <$> execRWST m Nothing (IRBuilderState 0 0 mempty mempty partialBlock)
  previousBlocks <- fmap (flip DList.apply mempty . basicBlocks) result
  currentBlk <- fmap currentBlock result
  pure $ previousBlocks ++ [partialBlockToBasicBlock currentBlk]

partialBlockToBasicBlock :: PartialBlock -> BasicBlock
partialBlockToBasicBlock pb =
  let currentTerm = fromMaybe (Terminator $ Ret Nothing) $ getFirst $ pbTerminator pb
  in BB (pbName pb) (pbInstructions pb) currentTerm

runIRBuilder :: IRBuilder a -> [BasicBlock]
runIRBuilder = runIdentity . runIRBuilderT

data Definition = Function Name [Type] [BasicBlock]
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

add :: Monad m => Operand -> Operand -> IRBuilderT m Operand
add lhs rhs =
  emitInstr $ Add lhs rhs

emitInstr :: Monad m => IR -> IRBuilderT m Operand
emitInstr instr = do
  operand <- mkOperand
  modify (addInstrToCurrentBlock operand)
  pure operand
  where
    addInstrToCurrentBlock operand s =
      -- TODO: record dot syntax? or create a helper function if this pattern occurs a lot..
      let instrs = DList.snoc (pbInstructions . currentBlock $ s) (operand, instr)
       in s { currentBlock = (currentBlock s) { pbInstructions = instrs } }

-- TODO: use local, and create fresh + use from mkOperand
named :: Monad m => IRBuilderT m a -> Name -> IRBuilderT m a
m `named` name = local (const $ Just name) m

block :: Monad m => IRBuilderT m Name
block = do
  let freshBlockName = ask >>= \case
        Nothing -> do
          name <- fresh
          pure $ Name $ "block_" <> unName name
        Just _sugg ->
          fresh
  blockName <- freshBlockName

  modify $ \s ->
    let currBlock = currentBlock s
        blocks = basicBlocks s
     in s { basicBlocks = DList.snoc blocks (partialBlockToBasicBlock currBlock)
          , currentBlock = PartialBlock blockName mempty mempty  -- TODO: use counter
          }
  pure blockName

  -- count <- gets blockCounter
  -- let blockName = Name $ "block_" <> T.pack (show count)
  -- modify $ \s ->
  --   let currBlock = currentBlock s
  --       blocks = basicBlocks s
  --    in s { basicBlocks = DList.snoc blocks (partialBlockToBasicBlock currBlock)
  --         , currentBlock = PartialBlock blockName mempty mempty  -- TODO: use counter
  --         , blockCounter = count + 1
  --         }
  -- pure blockName

-- TODO: turn into separate monad transformer ("NameSupply")
fresh :: Monad m => IRBuilderT m Name
fresh = ask >>= \case
  Nothing -> do
    count <- gets operandCounter
    modify $ \s -> s { operandCounter = count + 1 }
    pure $ Name $ T.pack (show count)
  Just suggestion -> do
    nameMapping <- gets nameMap
    let mCount = M.lookup suggestion nameMapping
        count = fromMaybe 0 mCount
    modify $ \s -> s { nameMap = M.insert suggestion (count + 1) nameMapping }
    pure $ Name $ unName suggestion <> "_" <> T.pack (show count)

-- NOTE: Only used internally, this creates an unassigned operand
mkOperand :: Monad m => IRBuilderT m Operand
mkOperand = do
  name <- fresh
  pure $ LocalRef name

data Type = IntType Int
  deriving Show

function :: Monad m => Name -> [Type] -> ([Operand] -> IRBuilderT (ModuleBuilderT m) a) -> ModuleBuilderT m Operand
function name tys fnBody = do
  instrs <- runIRBuilderT $ do
    operands <- traverse (const mkOperand) tys
    fnBody operands
  modify $ (Function name tys instrs:)
  pure $ GlobalRef name

exampleIR :: [BasicBlock]
exampleIR = runIRBuilder $ mdo
  let a = LocalRef "a"
  let b = LocalRef "b"
  d <- add a c
  c <- add a b
  pure d

exampleModule :: [Definition]
exampleModule = runModuleBuilder $ do
  function (Name "do_add") [IntType 1, IntType 32] $ \[x, y] -> mdo
    z <- add x y
    flip named "bla" $ do
      add x y
      add x y
      add x y

    block `named` "does this work?"
    block
    block
    _ <- add y z
    add y z

pp :: [Definition] -> Text
pp defs =
  T.unlines $ map ppDefinition defs

ppDefinition :: Definition -> Text
ppDefinition = \case
  Function (Name name) argTys body ->
    "define external ccc void @" <> name <> "(" <> fold (L.intersperse ", " (zipWith ppArg [0..] argTys)) <> ")" <> "{\n" <>
      ppBody body <> "\n" <>
      "}"
    where
      ppArg i _argTy = ppOperand $ LocalRef $ Name $ T.pack (show i)
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
  Add a b -> "add " <> ppOperand a <> " " <> ppOperand b
  Ret term ->
    case term of
      Nothing -> "ret void"
      Just operand -> "ret " <> ppOperand operand  -- TODO how to get type info?

ppOperand :: Operand -> Text
ppOperand op = "%" <> unName (unOperand op)

-- $> putStrLn . Data.Text.unpack $ pp exampleModule
