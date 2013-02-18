{-# LANGUAGE TypeFamilies, PolyKinds #-}
{- |
   Module      : Data.Keyless.IntMap.Lazy
   Description : Lazy IntMap-based lookup tables.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

-}
module Data.Keyless.IntMap.Lazy where

import Prelude hiding (lookup, map)
import Data.Keyless
import qualified Data.IntMap.Lazy as M
import Control.DeepSeq(NFData(..))

-- Size is O(n) rather than O(1) as with Map

-- -----------------------------------------------------------------------------

data KeylessIntMap a = KM { table   :: !(M.IntMap a)
                          , nextKey :: {-# UNPACK #-} !Key
                          }
                     deriving (Eq, Ord, Show, Read)

instance Functor KeylessIntMap where
  fmap = mapKM
  {-# INLINE fmap #-}

instance (NFData a) => NFData (KeylessIntMap a) where
  rnf (KM tbl nk) = rnf tbl `seq` rnf nk

initKM :: KeylessIntMap a
initKM = KM M.empty initKey

insertKM :: a -> KeylessIntMap a -> (Key, KeylessIntMap a)
insertKM v (KM tbl k) = (k, KM tbl' k')
  where
    k' = succ k
    tbl' = M.insert k v tbl

deleteKM      :: Key -> KeylessIntMap a -> KeylessIntMap a
deleteKM k km = km { table = M.delete k $ table km }

lookupKM   :: Key -> KeylessIntMap a -> Maybe a
lookupKM k = M.lookup k . table

unsafeLookupKM   :: Key -> KeylessIntMap a -> a
unsafeLookupKM k = (M.! k) . table

hasEntryKM   :: Key -> KeylessIntMap a -> Bool
hasEntryKM k = M.member k . table

adjustKM :: (a -> a) -> Key -> KeylessIntMap a -> KeylessIntMap a
adjustKM f k km = km { table = M.adjust f k $ table km }

sizeKM :: KeylessIntMap a -> Int
sizeKM = M.size . table

minKeyKM :: KeylessIntMap a -> Maybe Key
minKeyKM = fmap (fst . fst) . M.minViewWithKey . table

maxKeyKM :: KeylessIntMap a -> Maybe Key
maxKeyKM = fmap (fst . fst) . M.maxViewWithKey . table

-- Can use this for Map as we only store values we want.
isNullKM :: KeylessIntMap a -> Bool
isNullKM = M.null . table

keysKM :: KeylessIntMap a -> [Key]
keysKM = M.keys . table

valuesKM :: KeylessIntMap a -> [a]
valuesKM = M.elems . table

assocsKM :: KeylessIntMap a -> [(Key, a)]
assocsKM = M.assocs . table

fromListKM    :: [a] -> KeylessIntMap a
fromListKM vs = KM tbl nxtK
  where
    tbl = M.fromAscList $ zip [initKey..] vs
    nxtK = maybe initKey (succ . fst . fst) $ M.maxViewWithKey tbl

unsafeFromListWithKeysKM     :: [(Key, a)] -> KeylessIntMap a
unsafeFromListWithKeysKM kvs = KM tbl nxtK
  where
    tbl = M.fromList kvs -- Don't know if sorted
    nxtK = maybe initKey (succ . fst . fst) $ M.maxViewWithKey tbl

mergeKM :: KeylessIntMap a -> KeylessIntMap a
           -> ((Key -> Key), KeylessIntMap a)
mergeKM (KM tbl1 n1) (KM tbl2 n2) = (kf, KM tbl nxt)
  where
    kf = (+n1)
    tbl2' = M.mapKeysMonotonic kf tbl2
    tbl = M.union tbl1 tbl2'
    nxt = kf n2

differenceKM :: KeylessIntMap a -> KeylessIntMap a
                -> KeylessIntMap a
differenceKM km1 km2 = km1 { table = table km1 `M.difference` table km2 }

mapKM   :: (a -> b) -> KeylessIntMap a -> KeylessIntMap b
mapKM f = fmap f

mapWithKeyKM     :: (Key -> a -> b) -> KeylessIntMap a
                    -> KeylessIntMap b
mapWithKeyKM f km = km { table = M.mapWithKey f $ table km }

-- -----------------------------------------------------------------------------

instance Keyless KeylessIntMap where

  empty = initKM
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

  mapWithKey = mapWithKeyKM
  {-# INLINE mapWithKey #-}
