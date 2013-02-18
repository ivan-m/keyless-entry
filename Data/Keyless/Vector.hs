{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
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
import Data.Maybe(isNothing, isJust, catMaybes, fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad(liftM2, forM_)
import Control.Applicative((<|>),(<$>))
import Control.Arrow(second, (***))

-- -----------------------------------------------------------------------------

-- TODO: work out how to fix size on the type level (if we go that way)

-- If we cache valid Keys somehow, we can drop the usage of Maybe...
-- Using Set would give us log complexity back; another Vector would
-- take O(l) for getting all keys...

data KeylessVector a = KV { table      :: !(V.Vector (Maybe a))
                          , nextKey    :: {-# UNPACK #-} !Key
                            -- Invariant: for (Just (k1,k2)), k1 <= k2.
                          , bounds     :: !(Maybe (Key,Key))
                          -- Invariant: numVals >= 0; numVals == 0 iff bounds == Nothing
                          , numVals    :: {-# UNPACK #-} !Int
                          }
                     deriving (Eq, Ord, Show, Read)

-- We need c more values; increase if necessary or return original
-- vector.

instance Functor KeylessVector where
  fmap = mapKV
  {-# INLINE fmap #-}

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
    len' = (*) len . (^) (2::Int)
           . ceiling . logBase (2::Double)
           $ fromIntegral needLen / fromIntegral len
    missingLen = len' - len

boundCheck :: r -> Key -> KeylessVector a -> r -> r
boundCheck fl k kv act
  | k < initKey || k >= V.length (table kv) = fl
  | otherwise                               = act

-- By default, use doubling for vector size.

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
                        , bounds = Nothing
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
             , bounds = fmap (second $ const k) (bounds kv) <|> Just (k,k)
             , numVals = numVals kv + 1
             }

-- Need to read first to see if the numVal count should be adjusted.
deleteKV      :: Key -> KeylessVector a -> KeylessVector a
deleteKV k kv
  | isNothing $ bounds kv = kv -- No keys to worry about!
  | otherwise             = boundCheck kv k kv
                            $ if isNothing ma
                              then kv
                              else kv { table   = tbl'
                                      , bounds  = bnds'
                                      , numVals = numVals kv - 1
                                      }
  where
    tbl = table kv
    bnds@(Just (minK, maxK)) = bounds kv
    ma = tbl V.! k

    tbl' = V.modify (\mv -> MV.write mv k Nothing) tbl
    bnds'
      | minK == maxK = Nothing -- Implies ==k as well
      | minK == k    = (,maxK) <$> V.findIndex isJust tbl'
      | maxK == k    = (minK,) <$> V.findIndex isJust (V.reverse tbl')
      | otherwise    = bnds

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
minKeyKV = fmap fst . bounds

maxKeyKV :: KeylessVector a -> Maybe Key
maxKeyKV = fmap snd . bounds

-- Use default for isNull; other option is to see if bounds are Nothing.

keysKV :: KeylessVector a -> [Key]
keysKV = V.toList . V.findIndices isJust . table

valuesKV :: KeylessVector a -> [a]
valuesKV = catMaybes . V.toList . table

assocsKV :: KeylessVector a -> [(Key, a)]
assocsKV = catMaybes . V.toList . V.imap (fmap . (,)) . table

fromListKV :: [a] -> KeylessVector a
fromListKV xs
  | null xs   = initKV
  | otherwise = KV { table     = tbl
                   , nextKey   = len
                   , bounds    = Just (0, len-1)
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
                   , bounds  = Just (minK, maxK)
                   , numVals = cnt
                   }
  where
    ks = fmap fst kxs

    minK = minimum ks

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
                             , bounds  = bnds
                             , numVals = numVals kv1 + numVals kv2
                             }
  where
    tbl = V.modify (\mv -> V.unsafeCopy (MV.slice n1 len2 mv)
                           . V.slice 0 len2 $ table kv2)
          . checkSize len2 n1 $ table kv1

    n1 = nextKey kv1
    n2 = nextKey kv2

    len2 = n2 -- The "effective" length of kv2
    kf = (+n1)

    bnds = case (bounds kv1, fmap (kf *** kf) $ bounds kv2) of
                (Just (minK1,_), Just (_,maxK2)) -> Just (minK1, maxK2)
                (bnd1          , Nothing       ) -> bnd1
                (Nothing       , bnd2          ) -> bnd2

differenceKV :: KeylessVector a -> KeylessVector a -> KeylessVector a
differenceKV kv1 kv2 = maybe kv1 newKV $ mkRng bnds1 bnds2
  where
    bnds1 = bounds kv1
    bnds2 = bounds kv2

    tbl1 = table kv1
    tbl2 = table kv2

    mkRng = liftM2 $ rap . rap (max,min)

    newKV (minS,maxS)
      | V.null delIndices = kv1
      | otherwise         = kv1 { table = tbl'
                                , bounds = bnds'
                                , numVals = numV - numDel
                                }
      where
        delIndices = V.findIndices isJust . V.slice minS lenS $ tbl1
        lenS = max 0 $ maxS - minS + 1
        numDel = V.length . V.findIndices isJust $ V.unsafeBackpermute tbl2 delIndices

        tbl' = V.unsafeUpdate tbl1 $ V.map (,Nothing) delIndices

        -- Will be Just
        Just (min1,max1) = bnds1

        numV = numVals kv1

        bnds' = liftM2 (,) min' max'

        min' | numV == numDel         = Nothing
             | min1 < minS            = Just min1
             | isJust $ tbl' V.! min1 = Just min1
             | otherwise              = V.findIndex isJust tbl1Int

        max' | numV == numDel         = Nothing
             | max1 > maxS            = Just max1
             | isJust $ tbl' V.! max1 = Just max1
             | otherwise              = V.findIndex isJust . V.reverse $ tbl1Int

        -- The "interesting" bit.
        tbl1Int = V.slice min1 (max1 - min1 + 1) tbl1

    rap = uncurry (***)

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
