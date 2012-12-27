{-# LANGUAGE TypeFamilies, PolyKinds #-}
{- |
   Module      : Data.Keyless.Map.Lazy
   Description : Lazy Map-based lookup tables.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

-}
module Data.Keyless.Map.Lazy where

import Prelude hiding (lookup, map)
import Data.Keyless
import qualified Data.Map.Lazy as M

-- -----------------------------------------------------------------------------

data KeylessMap m a = KM { table   :: !(M.Map Key a)
                         , nextKey :: {-# UNPACK #-} !Key
                         }
                    deriving (Eq, Ord, Show, Read)

instance Functor (KeylessMap m) where
  fmap f km = km { table = fmap f $ table km }

initKM :: KeylessMap m a
initKM = KM M.empty initKey

insertKM :: (Monad m) => a -> KeylessMap m a -> m (Key, KeylessMap m a)
insertKM v (KM tbl k) = return (k, KM tbl' k')
  where
    k' = succ k
    tbl' = M.insert k v tbl

deleteKM      :: (Monad m) => Key -> KeylessMap m a -> m (KeylessMap m a)
deleteKM k km = return $ km { table = M.delete k $ table km }

lookupKM   :: (Monad m) => Key -> KeylessMap m a -> m (Maybe a)
lookupKM k = return . M.lookup k . table

unsafeLookupKM   :: (Monad m) => Key -> KeylessMap m a -> m a
unsafeLookupKM k = return . (M.! k) . table

hasEntryKM   :: (Monad m) => Key -> KeylessMap m a -> m Bool
hasEntryKM k = return . M.member k . table

adjustKM :: (Monad m) => (a -> a) -> Key -> KeylessMap m a -> m (KeylessMap m a)
adjustKM f k km = return $ km { table = M.adjust f k $ table km }

sizeKM :: (Monad m) => KeylessMap m a -> m Int
sizeKM = return . M.size . table

minKeyKM :: (Monad m) => KeylessMap m a -> m (Maybe Key)
minKeyKM = return . fmap (fst . fst) . M.minViewWithKey . table

maxKeyKM :: (Monad m) => KeylessMap m a -> m (Maybe Key)
maxKeyKM = return . fmap (fst . fst) . M.maxViewWithKey . table

-- Can use this for Map as we only store values we want.
isNullKM :: (Monad m) => KeylessMap m a -> m Bool
isNullKM = return . M.null . table

keysKM :: (Monad m) => KeylessMap m a -> m [Key]
keysKM = return . M.keys . table

valuesKM :: (Monad m) => KeylessMap m a -> m [a]
valuesKM = return . M.elems . table

assocsKM :: (Monad m) => KeylessMap m a -> m [(Key, a)]
assocsKM = return . M.assocs . table

fromListKM    :: (Monad m) => [a] -> m (KeylessMap m a)
fromListKM vs = return $ KM tbl nxtK
  where
    tbl = M.fromAscList $ zip [initKey..] vs
    nxtK = maybe initKey (succ . fst . fst) $ M.maxViewWithKey tbl

unsafeFromListWithKeysKM     :: (Monad m) => [(Key, a)] -> m (KeylessMap m a)
unsafeFromListWithKeysKM kvs = return $ KM tbl nxtK
  where
    tbl = M.fromList kvs -- Don't know if sorted
    nxtK = maybe initKey (succ . fst . fst) $ M.maxViewWithKey tbl

mergeKM :: (Monad m) => KeylessMap m a -> KeylessMap m a
           -> m ((Key -> Key), KeylessMap m a)
mergeKM (KM tbl1 n1) (KM tbl2 n2) = return (kf, KM tbl nxt)
  where
    kf = (+n1)
    tbl2' = M.mapKeysMonotonic kf tbl2
    tbl = M.union tbl1 tbl2'
    nxt = kf n2

differenceKM :: (Monad m) => KeylessMap m a -> KeylessMap m a
                -> m (KeylessMap m a)
differenceKM km1 km2 = return $ km1 { table = table km1 `M.difference` table km2 }

mapKM   :: (Monad m) => (a -> b) -> KeylessMap m a -> m (KeylessMap m b)
mapKM f = return . fmap f

mapWithKeyKM     :: (Monad m) => (Key -> a -> b) -> KeylessMap m a
                    -> m (KeylessMap m b)
mapWithKeyKM f km = return $ km { table = M.mapWithKey f $ table km }

-- -----------------------------------------------------------------------------

instance (Monad m) => Keyless (KeylessMap m) where
  type KMonad (KeylessMap m) = m

  empty = return initKM
  {-# INLINE empty #-}

  insert = insertKM
  {-# INLINE insert #-}

  delete = deleteKM
  {-# INLINE delete #-}

  lookup = lookupKM
  {-# INLINE lookup #-}

  unsafeLookup = unsafeLookupKM
  {-# INLINE unsafeLookup #-}

  hasEntry = hasEntryKM
  {-# INLINE hasEntry #-}

  adjust = adjustKM
  {-# INLINE adjust #-}

  size = sizeKM
  {-# INLINE size #-}

  minKey = minKeyKM
  {-# INLINE minKey #-}

  maxKey = maxKeyKM
  {-# INLINE maxKey #-}

  isNull = isNullKM
  {-# INLINE isNull #-}

  keys = keysKM
  {-# INLINE keys #-}

  values = valuesKM
  {-# INLINE values #-}

  assocs = assocsKM
  {-# INLINE assocs #-}

  fromList = fromListKM
  {-# INLINE fromList #-}

  unsafeFromListWithKeys = unsafeFromListWithKeysKM
  {-# INLINE unsafeFromListWithKeys #-}

  merge = mergeKM
  {-# INLINE merge #-}

  difference = differenceKM
  {-# INLINE difference #-}

  map = mapKM
  {-# INLINE map #-}

  mapWithKey = mapWithKeyKM
  {-# INLINE mapWithKey #-}
