module LLVM.Codegen.ArrayList
  ( ArrayList
  , new
  , append
  , toVector
  , isEmpty
  ) where

import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V

-- | Type that corresponds to array list in other languages.
--   Offers fast append access, by running in IO.
data ArrayList a = ArrayList !Int !(MV.IOVector a)

-- NOTE: only allocates memory, not filled with data yet!
new :: Word -> IO (ArrayList a)
new = fmap (ArrayList 0) . MV.unsafeNew . fromIntegral
{-# INLINE new #-}

append :: a -> ArrayList a -> IO (ArrayList a)
append a lst = do
  ArrayList pos vec <- growIfNeeded lst
  MV.unsafeWrite vec pos a
  pure $ ArrayList (pos + 1) vec
{-# INLINE append #-}

growIfNeeded :: ArrayList a -> IO (ArrayList a)
growIfNeeded a@(ArrayList pos vec) =
  if pos == MV.length vec
    then grow a
    else pure a
{-# INLINE growIfNeeded #-}

grow :: ArrayList a -> IO (ArrayList a)
grow (ArrayList pos vec) = do
  vec' <- MV.grow vec (pos * 2)
  pure $ ArrayList pos vec'
{-# INLINE grow #-}

toVector :: ArrayList a -> IO (V.Vector a)
toVector (ArrayList pos vec) =
  V.take (max 0 (pos - 1)) <$> V.unsafeFreeze vec
{-# INLINE toVector #-}

isEmpty :: ArrayList a -> Bool
isEmpty (ArrayList pos _) = pos == 0
{-# INLINE isEmpty #-}
