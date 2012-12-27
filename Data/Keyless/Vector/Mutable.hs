{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}
{- |
   Module      : Data.Keyless.Vector.Mutable
   Description : Lazy Map-based lookup tables.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

-}
module Data.Keyless.Vector.Mutable where

import Prelude hiding (lookup, map)

import Data.Keyless
import Data.Maybe(isNothing)
import qualified Data.Vector.Mutable as V
import qualified Data.Vector as PV
import Control.Monad.Primitive.Class
import Control.Monad.Primitive
import Control.Monad(liftM, liftM2, forM_)
import Control.Applicative((<|>))
import Control.Arrow(second, (***))

-- For unboxed vectors, clear is a no-op and thus unsafeLookup may
-- return a value even when there isn't one.

-- -----------------------------------------------------------------------------

-- TODO: work out how to fix size on the type level (if we go that way)

-- If we cache valid Keys somehow, we can drop the usage of Maybe...
-- Using Set would give us log complexity back; another Vector would
-- take O(l) for getting all keys...

data KeylessMVector m a = KV { table      :: !(V.MVector (StatePrim m) a)
                             , validKeys  :: !(V.MVector (StatePrim m) Bool)
                             , nextKey    :: {-# UNPACK #-} !Key
                             -- , fixSize :: !(Maybe Int)
                               -- Invariant: for (Just (k1,k2)), k1 <= k2.
                             , bounds     :: !(Maybe (Key,Key))
                             -- Invariant: numVals >= 0; numVals == 0 iff bounds == Nothing
                             , numVals    :: {-# UNPACK #-} !Int
                             }

-- We need c more values; increase if necessary or return original
-- vector.
checkSize :: (MonadPrim m) => Int -> KeylessMVector m a
             -> BaseMonad m (KeylessMVector m a)
checkSize c kv
  | needLen <= len = return kv
  | otherwise      = do v'  <- growVec v
                        vk' <- growVec vk
                        V.set (V.unsafeSlice len missingLen vk') False
                        return $ kv { table = v'
                                    , validKeys = vk'
                                    }
  where
    k = nextKey kv
    v = table kv
    vk = validKeys kv

    needLen = c + k
    len = V.length v -- also length of vk
    -- Wanting length to always be 2^n * startSize
    len' = (*) len . (^) (2::Int)
           . ceiling . logBase (2::Double)
           $ fromIntegral needLen / fromIntegral len
    missingLen = len' - len

    growVec :: (PrimMonad pm)
               => V.MVector (PrimState pm) b -> pm (V.MVector (PrimState pm) b)
    growVec = flip V.unsafeGrow missingLen

boundCheck :: (MonadPrim m) => r -> Key -> KeylessMVector m a
              -> m r -> m r
boundCheck fl k kv act
  | k < initKey || k >= V.length (table kv) = return fl
  | otherwise                               = act

-- By default, use doubling for vector size.

startSize :: Int
startSize = 10

-- | Create an empty 'KeylessMVector' of the specified initial
--   starting size rather than the default of @10@.
--
--   By creating a value of the required size up-front when you know
--   how big your data set is likely to be, you can avoid
--   re-allocation when the table runs out of space.
emptySized :: (MonadPrim m) => Int -> m (KeylessMVector m a)
emptySized = liftPM . emptySizedBase . max 1
-- Having an initial size of 1 is pretty stupid, but don't want to
-- "require" having a minimum of size 10.

emptySizedBase     :: (MonadPrim m) => Int -> BaseMonad m (KeylessMVector m a)
emptySizedBase len = do v <- V.unsafeNew len
                        vk <- V.replicate len False
                        return $ KV { table = v
                                    , validKeys = vk
                                    , nextKey = initKey
                                    , bounds = Nothing
                                    , numVals = 0
                                    }

freezeKeys :: (MonadPrim m) => KeylessMVector m a -> BaseMonad m (PV.Vector Bool)
freezeKeys = PV.freeze . validKeys

usedKeys :: (MonadPrim m) => KeylessMVector m a -> BaseMonad m (PV.Vector Int)
usedKeys = liftM (PV.findIndices id) . freezeKeys

-- -----------------------------------------------------------------------------

initKV :: (MonadPrim m) => m (KeylessMVector m a)
initKV = emptySized startSize

insertKV :: (MonadPrim m) => a -> KeylessMVector m a -> m (Key, KeylessMVector m a)
insertKV v kv = liftPM $ do kv'' <- checkSize 1 kv'
                            V.unsafeWrite (table kv'') k v
                            V.unsafeWrite (validKeys kv'') k True
                            return $ (k, kv'' { nextKey = k + 1 })
  where
    k = nextKey kv

    -- Can't set nextKey here because the `k' value is needed by checkSize.
    kv' = kv { bounds = fmap (second $ const k) (bounds kv) <|> Just (k,k)
             , numVals = numVals kv + 1
             }

-- Need to read first to see if the numVal count should be adjusted.
deleteKV      :: (MonadPrim m) => Key -> KeylessMVector m a
                 -> m (KeylessMVector m a)
deleteKV k kv
  | isNothing $ bounds kv = return kv -- No keys to worry about!
  | otherwise             = boundCheck kv k kv . liftPM $ do
    isV <- V.unsafeRead (validKeys kv) k
    if not isV
      then return kv
      else do
        -- Use kv' from here on!
        V.unsafeWrite (validKeys kv') k False
        V.clear $ V.unsafeSlice k 1 (table kv')
        -- This assumes that the key tables are correct.
        if | minK == maxK -> return $ kv' { bounds = Nothing } -- No other possible keys!
           | minK == k    -> do vksP <- freezeKeys kv'
                                let Just minK' = PV.findIndex id vksP
                                return $ kv' { bounds = Just (minK', maxK) }
           | maxK == k    -> do vksP <- freezeKeys kv'
                                let Just maxK' = PV.findIndex id $ PV.reverse vksP
                                return $ kv' { bounds = Just (minK, maxK') }
           | otherwise    -> return kv' -- deleted some middle value
  where
    kv'= kv { numVals = numVals kv - 1 }

    Just (minK, maxK) = bounds kv

lookupKV :: (MonadPrim m) => Key -> KeylessMVector m a -> m (Maybe a)
lookupKV k kv = boundCheck Nothing k kv . liftPM
                $ do isV <- V.unsafeRead (validKeys kv) k
                     if isV
                       then liftM Just $ V.unsafeRead (table kv) k
                       else return Nothing

-- Not using the default to avoid doing bound-checking, etc.
unsafeLookupKV   :: (MonadPrim m) => Key -> KeylessMVector m a -> m a
unsafeLookupKV k = liftPM . flip V.unsafeRead k . table

hasEntryKV      :: (MonadPrim m) => Key -> KeylessMVector m a -> m Bool
hasEntryKV k kv = boundCheck False k kv . liftPM
                  $ V.unsafeRead (validKeys kv) k

-- What happens if the function is strict and the value is undefined?
adjustKV :: (MonadPrim m) => (a -> a) -> Key -> KeylessMVector m a
            -> m (KeylessMVector m a)
adjustKV f k kv = boundCheck kv k kv . liftPM
                  $ do v <- V.unsafeRead (table kv) k
                       V.unsafeWrite (table kv) k (f v)
                       return kv

sizeKV :: (MonadPrim m) => KeylessMVector m a -> m Int
sizeKV = return . numVals

minKeyKV :: (MonadPrim m) => KeylessMVector m a -> m (Maybe Key)
minKeyKV = return . fmap fst . bounds

maxKeyKV :: (MonadPrim m) => KeylessMVector m a -> m (Maybe Key)
maxKeyKV = return . fmap snd . bounds

-- Use default for isNull; other option is to see if bounds are Nothing.

keysKV :: (MonadPrim m) => KeylessMVector m a -> m [Key]
keysKV = liftPM . liftM PV.toList . usedKeys

valuesKV    :: (MonadPrim m) => KeylessMVector m a -> m [a]
valuesKV kv = liftPM $ do ks <- usedKeys kv
                          as <- PV.freeze $ table kv
                          let as' = PV.backpermute as ks
                          return $ PV.toList as'

-- Don't use PV.indexed, as that excludes Prim and Storable-based vectors.
-- Do the work again to avoid calculating usedKeys twice.
assocsKV    :: (MonadPrim m) => KeylessMVector m a -> m [(Key, a)]
assocsKV kv = liftPM $ do ks <- usedKeys kv
                          as <- PV.freeze $ table kv
                          let as' = PV.toList $ PV.backpermute as ks
                              ks' = PV.toList ks
                          return $ zip ks' as'

fromListKV :: (MonadPrim m) => [a] -> m (KeylessMVector m a)
fromListKV xs
  | null xs   = initKV
  | otherwise = liftPM $ do tbl <- PV.thaw xsV
                            ks <- V.replicate len True
                            return $ KV { table     = tbl
                                        , validKeys = ks
                                        , nextKey   = len
                                        , bounds    = Just (0, len-1)
                                        , numVals   = len
                                        }
  where
    xsV = PV.fromList xs
    len = PV.length xsV

unsafeFromListWithKeysKV :: (MonadPrim m) => [(Key,a)]
                            -> m (KeylessMVector m a)
unsafeFromListWithKeysKV kxs
  | null kxs  = initKV
  | otherwise = liftPM $ do kv <- emptySizedBase len
                            forM_ kxs $ \ (k,x) -> do
                              V.unsafeWrite (table kv) k x
                              V.unsafeWrite (validKeys kv) k True
                            return $ kv { nextKey = len
                                        , bounds  = Just (minK, maxK)
                                        , numVals = cnt
                                        }
  where
    ks = fmap fst kxs

    minK = minimum ks

    maxK = maximum ks

    len = maxK + 1

    cnt = length kxs

-- Keep up to the last value that /might/ have been in either one,
-- even if deleted (for the sake of consistency)
mergeKV :: (MonadPrim m) => KeylessMVector m a -> KeylessMVector m a
           -> m ((Key -> Key), KeylessMVector m a)
mergeKV kv1 kv2 = liftPM
  $ do kv <- checkSize len2 kv1
       V.unsafeCopy (V.slice n1 len2 $ table kv)
                    (V.slice 0 len2  $ table kv2)
       V.unsafeCopy (V.slice n1 len2 $ validKeys kv)
                    (V.slice 0 len2  $ validKeys kv2)
       return . (,) kf $ kv { nextKey = kf n2 -- Even if n2 == 0, this will be equal to n1
                            , bounds  = bnds
                            , numVals = numVals kv1 + numVals kv2
                            }
  where
    n1 = nextKey kv1
    n2 = nextKey kv2

    len2 = n2 -- The "effective" length of kv2
    kf = (+n1)

    bnds = case (bounds kv1, fmap (kf *** kf) $ bounds kv2) of
                (Just (minK1,_), Just (_,maxK2)) -> Just (minK1, maxK2)
                (bnd1          , Nothing       ) -> bnd1
                (Nothing       , bnd2          ) -> bnd2

differenceKV :: (MonadPrim m) => KeylessMVector m a -> KeylessMVector m a
                -> m (KeylessMVector m a)
differenceKV kv1 kv2 = liftPM
  $ do ksDel <- liftM (PV.findIndices id) . PV.freeze $ V.slice 0 wantedLen keys2
       PV.forM_ ksDel $ \ k -> do
         -- No need for bound checking because of the slice
         V.unsafeWrite keys1 k False
         V.clear . V.slice k 1 $ table kv1
       ks <- usedKeys kv1 -- Relying upon mutation
       let numVs = PV.length ks
           minK =  ks PV.!? 0
           maxK = ks PV.!? (numVs - 1)
           bnds = liftM2 (,) minK maxK
       return $ kv1 { bounds = bnds
                    , numVals = numVs
                    }
  where
    len1 = V.length keys1
    len2 = V.length keys2
    wantedLen = len1 `min` len2

    keys2 = validKeys kv2
    keys1 = validKeys kv1

mapKV      :: (MonadPrim m) => (a -> b) -> KeylessMVector m a
              -> m (KeylessMVector m b)
mapKV f kv = liftPM . liftM (\ tbl -> kv { table = tbl } )
             $ PV.unsafeThaw . PV.map f
             =<< PV.unsafeFreeze (table kv)

mapWithKeyKV :: (MonadPrim m) => (Key -> a -> b) -> KeylessMVector m a
                -> m (KeylessMVector m b)
mapWithKeyKV f kv = liftPM . liftM (\ tbl -> kv { table = tbl } )
                    $ PV.unsafeThaw . PV.imap f
                    =<< PV.unsafeFreeze (table kv)


instance (MonadPrim m) => Keyless (KeylessMVector m) where
  type KMonad (KeylessMVector m) = m

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

  map = mapKV
  {-# INLINE map #-}

  mapWithKey = mapWithKeyKV
  {-# INLINE mapWithKey #-}
