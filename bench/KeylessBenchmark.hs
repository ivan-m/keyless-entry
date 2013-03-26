{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{- |
   Module      : KeylessBenchmark
   Description : Benchmarks for the keyless-entry library
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

-}
module Main where

{-

Benchmarks still to do:

* Mixed

* Delete non-existent

-}

import Prelude hiding (lookup, map)
import Data.List(foldl')
import Data.Keyless
import qualified Data.Keyless.IntMap.Lazy as IL
import qualified Data.Keyless.Map.Lazy as ML
import qualified Data.Keyless.Vector as V

import Criterion
import Criterion.Main(defaultMainWith)
import Criterion.Config(Config(..), defaultConfig, ljust)
import Control.DeepSeq

import Data.Functor((<$>))
import Control.Exception(evaluate)
import Control.Monad(join)
import Control.Monad.Trans(liftIO)

-- -----------------------------------------------------------------------------

main :: IO ()
main = defaultMainWith myConfig
       (liftIO . evaluate $ rnf [ B fromListSample
                                , B fromListSparseSample
                                , B emptyKML
                                , B emptyKIL
                                , B emptyKV
                                , B emptySmallKV
                                , B emptyJustTooSmallKV
                                , B emptyLargeKV
                                , B sampleKML
                                , B sampleSKML
                                , B sampleKIL
                                , B sampleSKIL
                                , B sampleKV
                                , B sampleSKV
                                , B sampleLargeKV
                                , B sampleLargeSKV
                                , B szdKML
                                , B szdSKML
                                , B szdKIL
                                , B szdSKIL
                                , B szdKV
                                , B szdSKV
                                , B szdLargeKV
                                , B szdLargeSKV
                                ])
       myBenchmarks

myBenchmarks :: [Benchmark]
myBenchmarks =
  [ bgroup "Creation"
      [ bench "Map.Lazy" $ nf (benchCreation fakeKML) fromListSample
      , bench "IntMap.Lazy" $ nf (benchCreation fakeKIL) fromListSample
      , bench "Vector" $ nf (benchCreation fakeKV) fromListSample
      ]

  , bgroup "Sparse creation"
      [ bench "Map.Lazy" $ nf (benchSparseCreation fakeKML) fromListSparseSample
      , bench "IntMap.Lazy" $ nf (benchSparseCreation fakeKIL) fromListSparseSample
      , bench "Vector" $ nf (benchSparseCreation fakeKV) fromListSparseSample
      ]

  , bgroup "Adding elements"
      [ bench "Map.Lazy" $ nf benchAddElem' emptyKML
      , bench "IntMap.Lazy" $ nf benchAddElem' emptyKIL
      , bench "default Vector" $ nf benchAddElem' emptyKV
      , bench "small Vector" $ nf benchAddElem' emptySmallKV
      , bench "just too small Vector" $ nf benchAddElem' emptyJustTooSmallKV
      , bench "large Vector" $ nf benchAddElem' emptyLargeKV
      ]

  , bgroup "Adding elements in bulk"
      [ bench "Map.Lazy" $ nf benchAddElemBulk' emptyKML
      , bench "IntMap.Lazy" $ nf benchAddElemBulk' emptyKIL
      , bench "default Vector" $ nf benchAddElemBulk' emptyKV
      , bench "small Vector" $ nf benchAddElemBulk' emptySmallKV
      , bench "just too small Vector" $ nf benchAddElemBulk' emptyJustTooSmallKV
      , bench "large Vector" $ nf benchAddElemBulk' emptyLargeKV
      ]

  , mkSimpleBenchGroup "Finding number of elements" benchSize

  , mkSimpleBenchGroup "Finding the minimum used key" benchMinKey

  , mkSimpleBenchGroup "Finding the maximum used key" benchMaxKey

  , mkSimpleBenchGroup "Finding all keys" benchKeys

  , mkSimpleBenchGroup "Finding all values" benchValues

  , mkSimpleBenchGroup "Finding all (key,value) pairs" benchAssocs

  , mkSimpleBenchGroup' "Merging with itself" benchMergeSame

  , mkSimpleBenchGroup' "Merging with empty (front)" benchMergeEmptyFront

  , mkSimpleBenchGroup' "Merging with empty (back)" benchMergeEmptyBack

  , mkSimpleBenchGroup' "Difference with itself" benchDifferenceSame

  , mkSimpleBenchGroup' "Difference with empty (front)" benchDifferenceEmptyFront

  , mkSimpleBenchGroup' "Difference with empty (back)" benchDifferenceEmptyBack

  , mkSimpleBenchGroup' "Mapping" benchMap

  , mkSimpleBenchGroup' "Mapping with keys" benchMapWithKey

  , mkSimpleBenchGroup "Checking for keys" benchHasKey

  , mkSizedBenchGroup' "Deleting from the middle" benchDelMid

  , mkSizedBenchGroup' "Deleting from the front" benchDelFront

  , mkSizedBenchGroup' "Deleting from the back" benchDelBack

  , mkSizedBenchGroup "Looking up keys" benchLookup
  ]

myConfig :: Config
myConfig = defaultConfig { cfgSamples = ljust 100 -- 0
                         -- , cfgPerformGC = ljust True
                         , cfgReport    = ljust "keylessBenchmark.html"
                         }

data K where
  K :: (NFKeyless c) => c Int -> K

instance NFData K where
  rnf (K k) = rnf k

mkSimpleBenchGroup' :: String -> (forall c . (NFKeyless c) => c Int -> c Int)
                       -> Benchmark
mkSimpleBenchGroup' nm f = mkSimpleBenchGroup nm $ K . f

mkSimpleBenchGroup :: (NFData r) => String
                      -> (forall c. (NFKeyless c) => c Int -> r) -> Benchmark
mkSimpleBenchGroup title f = bgroup title $ fmap toBnch ks
  where
    toBnch (nm,k) = bench nm . appK (nf f) $ k

    ks = [ ("Map.Lazy", K sampleKML)
         , ("sparse Map.Lazy", K sampleSKML)
         , ("IntMap.Lazy", K sampleKIL)
         , ("sparse IntMap.Lazy", K sampleSKIL)
         , ("default Vector", K sampleKV)
         , ("sparse default Vector", K sampleSKV)
         , ("large Vector", K sampleLargeKV)
         , ("sparse large Vector", K sampleLargeSKV)
         ]

appK :: (forall c. (NFKeyless c) => c Int -> a) -> K -> a
appK f (K c) = f c

data KS where
  KS :: (NFKeyless c) => SizedKeyless c -> KS

instance NFData KS where
  rnf (KS ks) = rnf ks

mkSizedBenchGroup' :: String -> (forall c . (NFKeyless c) => SizedKeyless c -> c Int)
                      -> Benchmark
mkSizedBenchGroup' nm f = mkSizedBenchGroup nm $ K . f

mkSizedBenchGroup :: (NFData r) => String
                     -> (forall c. (NFKeyless c) => SizedKeyless c -> r) -> Benchmark
mkSizedBenchGroup title f = bgroup title $ fmap toBnch ks
  where
    toBnch (nm,k) = bench nm . appKS (nf f) $ k

    ks = [ ("Map.Lazy", KS szdKML)
         , ("sparse Map.Lazy", KS szdSKML)
         , ("IntMap.Lazy", KS szdKIL)
         , ("sparse IntMap.Lazy", KS szdSKIL)
         , ("default Vector", KS szdKV)
         , ("sparse default Vector", KS szdSKV)
         , ("large Vector", KS szdLargeKV)
         , ("sparse large Vector", KS szdLargeSKV)
         ]

appKS :: (forall c. (NFKeyless c) => SizedKeyless c -> a) -> KS -> a
appKS f (KS c) = f c

-- -----------------------------------------------------------------------------

type (NFKeyless c) = (Keyless c, NFData (c Int))

-- | Provided value just for typing.
benchCreation   :: (Keyless c) => c Int -> [Int] -> c Int
benchCreation _ = fromList

-- | Provided value just for typing
benchSparseCreation   :: (Keyless c) => c Int -> [(Key,Int)] -> c Int
benchSparseCreation _ = unsafeFromListWithKeys

-- | Add @n@ values to the Keyless table.
--
--   Takes a 'Keyless' so that different 'V.KeylessVector' stages can
--   be compared.
--
--   When using as a benchmark, the intention is to have the @n@ value
--   static and applied to different keyless types.
benchAddElem :: (Keyless c) => Int -> c Int -> c Int
benchAddElem n c = foldl' (uncurry seq .: flip insert) c [1..n]

benchAddElem' :: (Keyless c) => c Int -> c Int
benchAddElem' = benchAddElem sizeToAdd
{-# NOINLINE benchAddElem' #-}

benchAddElemBulk :: (Keyless c) => Int -> c Int -> ([Key],c Int)
benchAddElemBulk n c = insertBulk [1..n] c

benchAddElemBulk' :: (Keyless c) => c Int -> ([Key], c Int)
benchAddElemBulk' = benchAddElemBulk sizeToAdd
{-# NOINLINE benchAddElemBulk' #-}

-- | All of the instances should be O(1) due to caching, etc. but
--   let's check...
benchSize :: (Keyless c) => c Int -> Int
benchSize = size

benchMinKey :: (Keyless c) => c Int -> Maybe Key
benchMinKey = minKey

benchMaxKey :: (Keyless c) => c Int -> Maybe Key
benchMaxKey = maxKey

benchKeys :: (Keyless c) => c Int -> [Key]
benchKeys = keys

benchValues :: (Keyless c) => c Int -> [Int]
benchValues = values

benchAssocs :: (Keyless c) => c Int -> [(Key,Int)]
benchAssocs = assocs

-- | Merge a table with itself.
benchMergeSame :: (NFKeyless c) => c Int -> c Int
benchMergeSame = snd . join merge

-- | Merge with an empty table for the first value
benchMergeEmptyFront :: (NFKeyless c) => c Int -> c Int
benchMergeEmptyFront = snd . merge empty

-- | Merge with an empty table for the second value
benchMergeEmptyBack :: (NFKeyless c) => c Int -> c Int
benchMergeEmptyBack = snd . (`merge` empty)

-- | Remove a table from itself.  Result should be an empty table.
benchDifferenceSame :: (NFKeyless c) => c Int -> c Int
benchDifferenceSame = join difference

benchDifferenceEmptyFront :: (NFKeyless c) => c Int -> c Int
benchDifferenceEmptyFront = difference empty

benchDifferenceEmptyBack :: (NFKeyless c) => c Int -> c Int
benchDifferenceEmptyBack = (`difference` empty)

benchMap :: (NFKeyless c) => c Int -> c Int
benchMap = map succ

benchMapWithKey :: (NFKeyless c) => c Int -> c Int
benchMapWithKey = mapWithKey (+)

-- | Checks a few keys.
benchHasKey :: (NFKeyless c) => c Int -> [Bool]
benchHasKey c = (`hasEntry` c) <$> ks
  where
    maxK = wantedSize * wantedGap -- Just to have a large number of keys to check
    ks = [0..maxK]

-- -----------------------------------------------------------------------------

-- For Benchmarks that need to know the size
data SizedKeyless c = SK { wKeyless :: !(c Int)
                         , wSize    :: !Int
                         , wKeys    :: ![Key]
                         }
-- Can't have this, as Keyless values are kind * -> *
--                    deriving (Eq, Ord, Show, Read)

instance (NFData (c Int)) => NFData (SizedKeyless c) where
  rnf (SK c s ks) = rnf c `seq` rnf s `seq` rnf ks

-- | Delete all /interior/ keys.
benchDelMid :: (Keyless c) => SizedKeyless c -> c Int
benchDelMid (SK c _ ks) = foldl' (flip delete) c delKs
  where
    -- All the interior ones
    delKs = init $ tail ks

-- | Delete every key, starting from the front.  Used to benchmark
--   'V.KeylessVector's performance as it has to scan for the new min
--   key.
benchDelFront :: (Keyless c) => SizedKeyless c -> c Int
benchDelFront (SK c _ ks) = foldl' (flip delete) c ks

-- | Delete every key, starting from the back.  Used to benchmark
--   'V.KeylessVector's performance as it has to scan for the new max
--   key.
benchDelBack :: (Keyless c) => SizedKeyless c -> c Int
benchDelBack (SK c _ ks) = foldl' (flip delete) c delKs
  where
    delKs = reverse ks

-- | Look up every key (unsafely).
benchLookup :: (Keyless c) => SizedKeyless c -> [Int]
benchLookup (SK c _ ks) = (c!) <$> ks

-- -----------------------------------------------------------------------------
-- Defined just for asTypeOf-like usage

fakeKML :: ML.KeylessMap Int
fakeKML = error "fakeKML"

fakeKIL :: IL.KeylessIntMap Int
fakeKIL = error "fakeKIL"

fakeKV :: V.KeylessVector Int
fakeKV = error "fakeKV"

-- -----------------------------------------------------------------------------
-- Data for creation purposes

wantedSize :: Int
wantedSize = 10

wantedGap :: Int
wantedGap = 10

fromListSample :: [Int]
fromListSample = [1..wantedSize]

fromListSparseSample :: [(Key,Int)]
fromListSparseSample = join (,) . (wantedGap*) <$> [1..wantedSize]
-- Note: not starting at 0!

-- -----------------------------------------------------------------------------
-- Various empty values to test for 'benchAddElem'

-- How many values to add.  By default, empty 'V.KeylessVector' values
-- are size 10, so use that (to avoid growing it).
sizeToAdd :: Int
sizeToAdd = 10

emptyKML :: ML.KeylessMap Int
emptyKML = empty

emptyKIL :: IL.KeylessIntMap Int
emptyKIL = empty

emptyKV :: V.KeylessVector Int
emptyKV = emptySized sizeToAdd

-- Smallest possible 'V.KeylessVector'; will need to be re-grown 4 times.
emptySmallKV :: V.KeylessVector Int
emptySmallKV = emptySized 1

-- /Just/ too small
emptyJustTooSmallKV :: V.KeylessVector Int
emptyJustTooSmallKV = emptySized $ pred sizeToAdd

-- A relatively large 'V.KeylessVector' with lots of needless copying.
emptyLargeKV :: V.KeylessVector Int
emptyLargeKV = emptySized $ wantedSize * wantedGap * 3 -- 1000
-- Factor of 3 so that it's too big even after merging.

-- -----------------------------------------------------------------------------

-- Create a packed and sparse value of each type for general testing.
mkSamples :: (Keyless c) => (c Int, c Int)
mkSamples = (fromList fromListSample, unsafeFromListWithKeys fromListSparseSample)

sampleKML, sampleSKML :: ML.KeylessMap Int
sampleKML = wKeyless szdKML
sampleSKML = wKeyless szdSKML

sampleKIL, sampleSKIL :: IL.KeylessIntMap Int
sampleKIL = wKeyless szdKIL
sampleSKIL = wKeyless szdSKIL

sampleKV, sampleSKV :: V.KeylessVector Int
sampleKV = wKeyless szdKV
sampleSKV = wKeyless szdSKV

-- Want variants with lots of extra space on the end
sampleLargeKV, sampleLargeSKV :: V.KeylessVector Int
sampleLargeKV  = wKeyless szdLargeKV
sampleLargeSKV = wKeyless szdLargeSKV

-- -----------------------------------------------------------------------------
-- Sized samples

mkSKs :: (Keyless c) => (SizedKeyless c, SizedKeyless c)
mkSKs = ( SK { wKeyless = fromList fromListSample
             , wSize    = wantedSize
             , wKeys    = [0..wantedSize - 1]
             }
        , SK { wKeyless = unsafeFromListWithKeys fromListSparseSample
             , wSize    = wantedSize
             , wKeys    = fst <$> fromListSparseSample
             }
        )

szdKML, szdSKML :: SizedKeyless ML.KeylessMap
(szdKML, szdSKML) = mkSKs

szdKIL, szdSKIL :: SizedKeyless IL.KeylessIntMap
(szdKIL, szdSKIL) = mkSKs

szdKV, szdSKV :: SizedKeyless V.KeylessVector
(szdKV, szdSKV) = mkSKs

-- Want variants with lots of extra space on the end
szdLargeKV, szdLargeSKV :: SizedKeyless V.KeylessVector
szdLargeKV  = szdKV  { wKeyless = snd . merge emptyLargeKV $ wKeyless szdKV  }
szdLargeSKV = szdSKV { wKeyless = snd . merge emptyLargeKV $ wKeyless szdSKV }

-- -----------------------------------------------------------------------------

-- | Taken from the benchmarks for unordered-containers
data B where
  B :: NFData a => a -> B

instance NFData B where
  rnf (B b) = rnf b

(.:)   :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f .: g = (f .) . g

infixr 9 .:
