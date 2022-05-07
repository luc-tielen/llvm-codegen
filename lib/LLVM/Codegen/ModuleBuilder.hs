module LLVM.Codegen.ModuleBuilder
  ( ModuleBuilderT
  , ModuleBuilder
  , runModuleBuilderT
  , runModuleBuilder
  , Module(..)
  , Definition(..)
  , function
  ) where

import Control.Monad.State
import qualified Data.Text as T
import Data.Functor.Identity
import LLVM.Codegen.IRBuilder
import LLVM.Codegen.Operand
import LLVM.Codegen.Type
import LLVM.NameSupply
import LLVM.Pretty


data Module
  = Module [Definition]

instance Pretty Module where
  pretty (Module defs) =
    vsep $ map pretty defs


data Definition = Function Name Type [Type] [BasicBlock]
  deriving Show

instance Pretty Definition where
 pretty = \case
   Function name retTy argTys body ->
     "define external ccc" <+> pretty retTy <+> fnName <> tupled (zipWith prettyArg [0..] argTys) <+>
       "{" <> hardline <>
       prettyBody body <> hardline <>
       "}"
    where
      fnName = "@" <> pretty name
      prettyArg :: Int -> Type -> Doc ann
      prettyArg i argTy = pretty $ LocalRef argTy $ Name $ T.pack (show i)
      prettyBody blocks = vsep $ map pretty blocks


type ModuleBuilderState = [Definition]

newtype ModuleBuilderT m a
  = ModuleBuilder (StateT ModuleBuilderState m a)
  deriving (Functor, Applicative, Monad, MonadState ModuleBuilderState, MonadFix)
  via StateT ModuleBuilderState m

type ModuleBuilder = ModuleBuilderT Identity

runModuleBuilderT :: Monad m => ModuleBuilderT m a -> m Module
runModuleBuilderT (ModuleBuilder m) =
  Module <$> execStateT m []

runModuleBuilder :: ModuleBuilder a -> Module
runModuleBuilder = runIdentity . runModuleBuilderT


function :: Monad m => Name -> [Type] -> Type -> ([Operand] -> IRBuilderT (ModuleBuilderT m) a) -> ModuleBuilderT m Operand
function name tys retTy fnBody = do
  instrs <- runIRBuilderT $ do
    operands <- traverse mkOperand tys
    fnBody operands
  modify $ (Function name retTy tys instrs:)
  pure $ GlobalRef (PointerType (FunctionType retTy tys)) name

-- NOTE: Only used internally, this creates an unassigned operand
mkOperand :: Monad m => Type -> IRBuilderT m Operand
mkOperand ty = do
  name <- fresh
  pure $ LocalRef ty name

