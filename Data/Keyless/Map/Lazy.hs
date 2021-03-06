{-# LANGUAGE FlexibleInstances, MonadComprehensions, MultiParamTypeClasses,
             TupleSections, TypeFamilies #-}

{- |
   Module      : Data.Keyless.Map.Lazy
   Description : Lazy Map-based lookup tables.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

-}
module Data.Keyless.Map.Lazy (KeylessMap) where

import           Control.DeepSeq (NFData (..))
import           Data.Keyless
import qualified Data.Map.Lazy   as M
import           Prelude         hiding (lookup, map)

-- -----------------------------------------------------------------------------

data KeylessMap a = KM { table   :: !(M.Map Key a)
                       , nextKey :: {-# UNPACK #-} !Key
                       }
                  deriving (Eq, Ord, Show, Read)

instance Functor KeylessMap where
  fmap = mapKM
  {-# INLINE fmap #-}

instance (NFData a) => NFData (KeylessMap a) where
  rnf (KM tbl nk) = rnf tbl `seq` rnf nk

initKM :: KeylessMap a
initKM = KM M.empty initKey

insertKM :: a -> KeylessMap a -> (Key, KeylessMap a)
insertKM v (KM tbl k) = (k, KM tbl' k')
  where
    k' = succ k
    tbl' = M.insert k v tbl

insertBulkKM :: [a] -> KeylessMap a -> ([Key], KeylessMap a)
insertBulkKM as (KM tbl nk) = (ks, KM tbl' $ nk + sz')
  where
    kas = M.fromAscList $ zip [nk..] as
    sz' = M.size kas
    ks = M.keys kas
    tbl' = tbl `M.union` kas

deleteKM      :: Key -> KeylessMap a -> KeylessMap a
deleteKM k km = km { table = M.delete k $ table km }

deleteBulkKM :: [Key] -> KeylessMap a -> KeylessMap a
deleteBulkKM ks km = km { table = table km `M.difference` ks' }
  where
    ks' = M.fromList $ fmap (,()) ks

lookupKM   :: Key -> KeylessMap a -> Maybe a
lookupKM k = M.lookup k . table

unsafeLookupKM   :: Key -> KeylessMap a -> a
unsafeLookupKM k = (M.! k) . table

hasEntryKM   :: Key -> KeylessMap a -> Bool
hasEntryKM k = M.member k . table

adjustKM :: (a -> a) -> Key -> KeylessMap a -> KeylessMap a
adjustKM f k km = km { table = M.adjust f k $ table km }

sizeKM :: KeylessMap a -> Int
sizeKM = M.size . table

minKeyKM :: KeylessMap a -> Maybe Key
minKeyKM = fmap (fst . fst) . M.minViewWithKey . table

maxKeyKM :: KeylessMap a -> Maybe Key
maxKeyKM = fmap (fst . fst) . M.maxViewWithKey . table

-- Can use this for Map as we only store values we want.
isNullKM :: KeylessMap a -> Bool
isNullKM = M.null . table

keysKM :: KeylessMap a -> [Key]
keysKM = M.keys . table

valuesKM :: KeylessMap a -> [a]
valuesKM = M.elems . table

assocsKM :: KeylessMap a -> [(Key, a)]
assocsKM = M.assocs . table

fromListKM    :: [a] -> KeylessMap a
fromListKM vs = KM tbl nxtK
  where
    tbl = M.fromAscList $ zip [initKey..] vs
    nxtK = maybe initKey (succ . fst . fst) $ M.maxViewWithKey tbl

unsafeFromListWithKeysKM     :: [(Key, a)] -> KeylessMap a
unsafeFromListWithKeysKM kvs = KM tbl nxtK
  where
    tbl = M.fromList kvs -- Don't know if sorted
    nxtK = maybe initKey (succ . fst . fst) $ M.maxViewWithKey tbl

mergeKM :: KeylessMap a -> KeylessMap a
           -> ((Key -> Key), KeylessMap a)
mergeKM (KM tbl1 n1) (KM tbl2 n2) = (kf, KM tbl nxt)
  where
    kf = (+n1)
    tbl2' = M.mapKeysMonotonic kf tbl2
    tbl = M.union tbl1 tbl2'
    nxt = kf n2

mergeAllKM :: [KeylessMap a] -> ([MergeTranslation Key], KeylessMap a)
mergeAllKM []  = ([], initKM)
mergeAllKM kvs = (mts, KM { table = tbl, nextKey = nk })
  where
    szs = fmap nextKey kvs
    fs = fmap (+) . scanl (+) 0 $ init szs
    tbl = M.unions . zipWith M.mapKeysMonotonic fs $ fmap table kvs

    mts = zipWith toMT fs szs

    toMT f nxtKey = MT { newBounds = [ (f initKey, f $ pred nxtKey)
                                       | nxtKey > initKey
                                     ]
                       , oldToNew = f
                       }

    nk = last $ zipWith ($) fs szs

differenceKM :: KeylessMap a -> KeylessMap a
                -> KeylessMap a
differenceKM km1 km2 = km1 { table = table km1 `M.difference` table km2 }

mapKM   :: (a -> b) -> KeylessMap a -> KeylessMap b
mapKM f km = km { table = fmap f $ table km }

mapWithKeyKM     :: (Key -> a -> b) -> KeylessMap a
                    -> KeylessMap b
mapWithKeyKM f km = km { table = M.mapWithKey f $ table km }

-- -----------------------------------------------------------------------------

instance Keyless (KeylessMap a) where

  type Elem (KeylessMap a) = a

  empty = initKM
  {-# INLINE empty #-}

  insert = insertKM
  {-# INLINE insert #-}

  insertBulk = insertBulkKM
  {-# INLINE insertBulk #-}

  delete = deleteKM
  {-# INLINE delete #-}

  deleteBulk = deleteBulkKM
  {-# INLINE deleteBulk #-}

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

  mergeAll = mergeAllKM
  {-# INLINE mergeAll #-}

  difference = differenceKM
  {-# INLINE difference #-}

instance FKeyless KeylessMap a where

  mapWithKey = mapWithKeyKM
  {-# INLINE mapWithKey #-}
