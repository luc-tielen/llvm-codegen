{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}

module LLVM.Codegen.NameSupply
  ( Name(..)
  , Counter
  , NameSupplyState(..)
  , NameSupplyT(..)
  , runNameSupplyT
  , MonadNameSupply(..)
  ) where

import qualified Control.Monad.RWS.Lazy as LazyRWS
import qualified Control.Monad.RWS.Strict as StrictRWS
import Control.Monad.RWS.Strict (RWST)
import qualified Control.Monad.State.Lazy as LazyState
import qualified Control.Monad.State.Strict as StrictState
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.Morph
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import LLVM.Codegen.Name


type Counter = Int

data NameSupplyState
  = NameSupplyState
  { counter :: Counter
  , nameMap :: Map Name Counter
  }

newtype NameSupplyT m a
  = NameSupplyT { unNameSupplyT :: RWST (Maybe Name) () NameSupplyState m a }
  deriving (Functor, Applicative, Monad, MonadReader (Maybe Name), StrictState.MonadState NameSupplyState, MonadFix, MonadIO)
  via RWST (Maybe Name) () NameSupplyState m

instance MonadTrans NameSupplyT where
  lift = NameSupplyT . lift

instance MFunctor NameSupplyT where
  hoist nat = NameSupplyT . hoist nat . unNameSupplyT

runNameSupplyT :: Monad m => NameSupplyT m a -> m a
runNameSupplyT (NameSupplyT m) =
  fst <$> StrictRWS.evalRWST m Nothing (NameSupplyState 0 mempty)

class Monad m => MonadNameSupply m where
  fresh :: m Name
  named :: m a -> Name -> m a
  getSuggestion :: m (Maybe Name)

  default fresh :: (MonadTrans t, MonadNameSupply m1, m ~ t m1) => m Name
  fresh = lift fresh

  default getSuggestion :: (MonadTrans t, MonadNameSupply m1, m ~ t m1) => m (Maybe Name)
  getSuggestion = lift getSuggestion

instance Monad m => MonadNameSupply (NameSupplyT m) where
  getSuggestion = ask

  fresh = getSuggestion >>= \case
    Nothing -> do
      count <- StrictState.gets counter
      StrictState.modify $ \s -> s { counter = count + 1 }
      pure $ Name $ T.pack (show count)
    Just suggestion -> do
      nameMapping <- StrictState.gets nameMap
      let mCount = M.lookup suggestion nameMapping
          count = fromMaybe 0 mCount
      StrictState.modify $ \s -> s { nameMap = M.insert suggestion (count + 1) nameMapping }
      pure $ Name $ unName suggestion <> "_" <> T.pack (show count)

  m `named` name =
    local (const $ Just name) m

instance MonadNameSupply m => MonadNameSupply (StrictState.StateT s m) where
  named = flip (StrictState.mapStateT . flip named)

instance MonadNameSupply m => MonadNameSupply (LazyState.StateT s m) where
  named = flip (LazyState.mapStateT . flip named)

instance (MonadNameSupply m, Monoid w) => MonadNameSupply (StrictRWS.RWST r w s m) where
  named = flip (StrictRWS.mapRWST . flip named)

instance (MonadNameSupply m, Monoid w) => MonadNameSupply (LazyRWS.RWST r w s m) where
  named = flip (LazyRWS.mapRWST . flip named)

instance MonadNameSupply m => MonadNameSupply (ReaderT r m) where
  named = flip (mapReaderT . flip named)

instance (MonadNameSupply m, Monoid w) => MonadNameSupply (WriterT w m) where
  named = flip (mapWriterT . flip named)

instance MonadNameSupply m => MonadNameSupply (ExceptT e m) where
  named = flip (mapExceptT . flip named)
