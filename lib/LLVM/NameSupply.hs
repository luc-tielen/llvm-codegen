-- TODO: move to Codegen folder
module LLVM.NameSupply
  ( Name(..)
  , Counter
  , NameSupplyState(..)
  , NameSupplyT(..)
  , runNameSupplyT
  , MonadNameSupply(..)
  ) where

import Control.Monad.RWS.Lazy
import Control.Monad.State.Lazy
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Text (Text)
import Data.Map (Map)
import Data.Maybe
import LLVM.Pretty


newtype Name = Name { unName :: Text }
  deriving (Eq, Ord, Show)

type Counter = Int

data NameSupplyState
  = NameSupplyState
  { counter :: Counter
  , nameMap :: Map Name Counter
  }

newtype NameSupplyT m a
  = NameSupplyT (RWST (Maybe Name) () NameSupplyState m a)
  deriving (Functor, Applicative, Monad, MonadReader (Maybe Name), MonadState NameSupplyState, MonadFix, MonadIO)
  via RWST (Maybe Name) () NameSupplyState m

runNameSupplyT :: Monad m => NameSupplyT m a -> m a
runNameSupplyT (NameSupplyT m) =
  fst <$> evalRWST m Nothing (NameSupplyState 0 mempty)

class Monad m => MonadNameSupply m where
  fresh :: m Name
  named :: m a -> Name -> m a

instance Monad m => MonadNameSupply (NameSupplyT m) where
  fresh = ask >>= \case
    Nothing -> do
      count <- gets counter
      modify $ \s -> s { counter = count + 1 }
      pure $ Name $ T.pack (show count)
    Just suggestion -> do
      nameMapping <- gets nameMap
      let mCount = M.lookup suggestion nameMapping
          count = fromMaybe 0 mCount
      modify $ \s -> s { nameMap = M.insert suggestion (count + 1) nameMapping }
      pure $ Name $ unName suggestion <> "_" <> T.pack (show count)

  m `named` name =
    local (const $ Just name) m

instance MonadNameSupply m => MonadNameSupply (StateT s m) where
  fresh = lift fresh
  named = flip $ (mapStateT . flip named)


instance Pretty Name where
  pretty (Name name) =
    pretty name
