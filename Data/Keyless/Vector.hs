{-# LANGUAGE TupleSections #-}
{- |
   Module      : Data.Keyless.Vector
   Description : Lazy Map-based lookup tables.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

-}
module Data.Keyless.Vector where

import Prelude hiding (lookup, map)

import Data.Keyless
import Data.Maybe(isNothing, isJust, fromMaybe, fromJust)
import Data.Monoid(mconcat)
import Data.Ord(comparing)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Fusion.Stream as VS
import Control.Applicative((<$>))
import Control.Monad(forM_)
import Control.DeepSeq(NFData(..))

-- -----------------------------------------------------------------------------

-- TODO: work out how to fix size on the type level (if we go that way)

-- If we cache valid Keys somehow, we can drop the usage of Maybe...
-- Using Set would give us log complexity back; another Vector would
-- take O(l) for getting all keys...

data KeylessVector a = KV { table      :: !(V.Vector (Maybe a))
                          , nextKey    :: {-# UNPACK #-} !Key
                          , numVals    :: {-# UNPACK #-} !Int
                          }
                     deriving (Show, Read)

instance (Eq a) => Eq (KeylessVector a) where
  (KV c1 k1 v1) == (KV c2 k2 v2) = v1 == v2
                                   && k1 == k2
                                   && getVals c1 == getVals c2
    where
      getVals = V.filter (isJust . snd) . V.indexed

instance (Ord a) => Ord (KeylessVector a) where
  compare = mconcat [ comparing numVals
                    , comparing nextKey
                    , comparing $ V.filter (isJust . snd) . V.indexed . table
                    ]

instance Functor KeylessVector where
  fmap = mapKV
  {-# INLINE fmap #-}

instance (NFData a) => NFData (KeylessVector a) where
  rnf (KV tbl nk nv) = rnf tbl `seq` rnf nk `seq` rnf nv

checkSize :: Int -> Key -> V.Vector (Maybe a) -> V.Vector (Maybe a)
checkSize c k v
  | needLen <= len = v
  | otherwise      = V.create
                     $ do mv <- V.thaw v
                          mv' <- MV.unsafeGrow mv missingLen
                          MV.set (MV.unsafeSlice len missingLen mv') Nothing
                          return mv'
  where
    needLen = c + k
    len = V.length v
    -- Wanting length to always be 2^n * startSize
    len' = (*) len . (^) growthFactor . (`asTypeOf`(undefined::Int))
           . ceiling . logBase (2::Double)
           $ fromIntegral needLen / fromIntegral len
    missingLen = len' - len

-- How much we should increase a Vector upon re-sizing.
growthFactor :: Int
growthFactor = 2

boundCheck :: r -> Key -> KeylessVector a -> r -> r
boundCheck fl k kv act
  | k < initKey || k >= V.length (table kv) = fl
  | otherwise                               = act

startSize :: Int
startSize = 10

-- | Create an empty 'KeylessVector' of the specified initial
--   starting size rather than the default of @10@.
--
--   By creating a value of the required size up-front when you know
--   how big your data set is likely to be, you can avoid
--   re-allocation when the table runs out of space.
emptySized :: Int -> KeylessVector a
emptySized = emptySizedBase . max 1
-- Having an initial size of 1 is pretty stupid, but don't want to
-- "require" having a minimum of size 10.

emptySizedBase     :: Int -> KeylessVector a
emptySizedBase len = KV { table = V.replicate len Nothing
                        , nextKey = initKey
                        , numVals = 0
                        }

-- -----------------------------------------------------------------------------

initKV :: KeylessVector a
initKV = emptySized startSize

insertKV :: a -> KeylessVector a -> (Key, KeylessVector a)
insertKV a kv = (k, kv')
  where
    k = nextKey kv
    v = table kv

    -- Using modify here rather than update because it will probably
    -- have been resized and thus fusion!
    v' = V.modify (\ mv -> MV.write mv k (Just a)) $ checkSize 1 k v

    kv' = kv { table = v'
             , nextKey = k + 1
             , numVals = numVals kv + 1
             }

-- Need to read first to see if the numVal count should be adjusted.
deleteKV :: Key -> KeylessVector a -> KeylessVector a
deleteKV k kv
  | numVals kv == 0 = kv -- No keys to worry about!
  | otherwise       = boundCheck kv k kv
                      $ if isNothing ma
                        then kv
                        else kv { table   = tbl'
                                , numVals = numVals kv - 1
                                }
  where
    tbl = table kv
    ma = tbl V.! k

    tbl' = V.modify (\mv -> MV.write mv k Nothing) tbl

lookupKV :: Key -> KeylessVector a -> Maybe a
lookupKV k kv = boundCheck Nothing k kv $ table kv V.! k

-- Not using the default to avoid doing bound-checking, etc.
unsafeLookupKV      :: Key -> KeylessVector a -> a
unsafeLookupKV k kv = fromMaybe err $ table kv V.! k
  where
    err = error $ "There is no value corresponding to the key `" ++ show k ++ "'\
                   \ in the provided KeylessVector."

hasEntryKV      :: Key -> KeylessVector a -> Bool
hasEntryKV k kv = boundCheck False k kv . isJust $ table kv V.! k


-- What happens if the function is strict and the value is undefined?
adjustKV :: (a -> a) -> Key -> KeylessVector a -> KeylessVector a
adjustKV f k kv = boundCheck kv k kv $ kv { table = v' }
  where
    v' = V.modify (\ mv -> MV.unsafeWrite mv k . fmap f =<< MV.unsafeRead mv k)
         $ table kv

sizeKV :: KeylessVector a -> Int
sizeKV = numVals

minKeyKV :: KeylessVector a -> Maybe Key
minKeyKV = V.findIndex isJust . table

maxKeyKV :: KeylessVector a -> Maybe Key
maxKeyKV kv = fmap fst . findR (isJust . snd)
              . V.indexed . V.unsafeSlice initKey (nextKey kv) $ table kv

findR :: (a -> Bool) -> V.Vector a -> Maybe a
findR p = VS.find p . VG.streamR

-- Use default for isNull; other option is to see if bounds are Nothing.

keysKV :: KeylessVector a -> [Key]
keysKV kv = V.toList . V.findIndices isJust
            . V.unsafeSlice initKey (nextKey kv) $ table kv

valuesKV :: KeylessVector a -> [a]
valuesKV kv = V.toList . V.map fromJust . V.filter isJust
              . V.unsafeSlice initKey (nextKey kv) $ table kv

assocsKV :: KeylessVector a -> [(Key, a)]
assocsKV kv = V.toList . V.map fromJust . V.filter isJust . V.imap (fmap . (,))
              . V.unsafeSlice initKey (nextKey kv) $ table kv

fromListKV :: [a] -> KeylessVector a
fromListKV xs
  | null xs   = initKV
  | otherwise = KV { table     = tbl
                   , nextKey   = len
                   , numVals   = len
                   }
  where
    tbl = V.fromList $ fmap Just xs
    len = V.length tbl

unsafeFromListWithKeysKV :: [(Key,a)] -> KeylessVector a
unsafeFromListWithKeysKV kxs
  | null kxs  = initKV
  | otherwise = KV { table = tbl
                   , nextKey = len
                   , numVals = cnt
                   }
  where
    ks = fmap fst kxs

    maxK = maximum ks

    len = maxK + 1

    cnt = length kxs

    tbl = V.modify (\mv -> forM_ kxs . uncurry $ (. Just) . MV.unsafeWrite mv)
          -- We know that len > 0
          $ V.replicate len Nothing

-- Keep up to the last value that /might/ have been in either one,
-- even if deleted (for the sake of consistency)
mergeKV :: KeylessVector a -> KeylessVector a -> ((Key -> Key), KeylessVector a)
mergeKV kv1 kv2 = (kf,) $ KV { table   = tbl
                             , nextKey = kf n2  -- Even if n2 == 0, this will be equal to n1
                             , numVals = numVals kv1 + numVals kv2
                             }
  where
    -- This implementation seems to be more efficient for compact vectors.
    -- tbl = V.unsafeSlice initKey n1 (table kv1)
    --       V.++ V.unsafeSlice initKey n2 (table kv2)

    tbl = V.modify (\mv -> V.unsafeCopy (MV.unsafeSlice n1 len2 mv)
                           . V.unsafeSlice initKey len2 $ table kv2)
          . checkSize len2 n1 $ table kv1

    n1 = nextKey kv1
    n2 = nextKey kv2

    len2 = n2 -- The "effective" length of kv2
    kf = (+n1)

differenceKV :: KeylessVector a -> KeylessVector a -> KeylessVector a
differenceKV kv1 kv2
  | isNull kv1 = kv1 -- Nothing to delete!
  | isNull kv2 = kv1 -- Not deleting anything!!
  | otherwise  = newKV
  where
    tbl1 = table kv1
    tbl2 = table kv2

    newKV
      | V.null delIndices = kv1
      | otherwise         = kv1 { table   = tbl'
                                , numVals = numV - numDel
                                }
      where
        delIndices = V.findIndices isJust $ V.unsafeSlice initKey (nextKey kv2) tbl2
        numDel = V.length . V.findIndices isJust $ V.unsafeBackpermute tbl1 delIndices

        tbl' = V.unsafeUpdate tbl1 $ V.map (,Nothing) delIndices

        numV = numVals kv1

mapKV      :: (a -> b) -> KeylessVector a -> KeylessVector b
mapKV f kv = kv { table = V.map (fmap f) $ table kv }

mapWithKeyKV      :: (Key -> a -> b) -> KeylessVector a -> KeylessVector b
mapWithKeyKV f kv = kv { table = V.imap (fmap . f) $ table kv }

instance Keyless KeylessVector where

  empty = initKV
  {-# INLINE empty #-}

  insert = insertKV
  {-# INLINE insert #-}

  delete = deleteKV
  {-# INLINE delete #-}

  lookup = lookupKV
  {-# INLINE lookup #-}

  unsafeLookup = unsafeLookupKV
  {-# INLINE unsafeLookup #-}

  hasEntry = hasEntryKV
  {-# INLINE hasEntry #-}

  adjust = adjustKV
  {-# INLINE adjust #-}

  size = sizeKV
  {-# INLINE size #-}

  minKey = minKeyKV
  {-# INLINE minKey #-}

  maxKey = maxKeyKV
  {-# INLINE maxKey #-}

  -- isNull = isNullKV
  -- {-# INLINE isNull #-}

  keys = keysKV
  {-# INLINE keys #-}

  values = valuesKV
  {-# INLINE values #-}

  assocs = assocsKV
  {-# INLINE assocs #-}

  fromList = fromListKV
  {-# INLINE fromList #-}

  unsafeFromListWithKeys = unsafeFromListWithKeysKV
  {-# INLINE unsafeFromListWithKeys #-}

  merge = mergeKV
  {-# INLINE merge #-}

  difference = differenceKV
  {-# INLINE difference #-}

  mapWithKey = mapWithKeyKV
  {-# INLINE mapWithKey #-}
