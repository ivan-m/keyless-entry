{- |
   Module      : Data.Keyless
   Description : Class of lookup tables with generated keys.
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

This module defines a class for when you want to store data in a
lookup table/dictionary but the type/value of the key isn't important.

-}
module Data.Keyless where

import Prelude hiding (lookup, map)
import Data.Maybe(isJust, fromMaybe)

-- -----------------------------------------------------------------------------

-- | The keys of the values in the 'Keyless' tables.  Note that all
--   keys will be @>= 0@ and no key will be repeated (even if the
--   value corresponding to the maximal key is deleted and then a new
--   value is added, a new higher key will be returned).
type Key = Int

-- | The minimum key used.
initKey :: Key
initKey = 0

class (Functor c) => Keyless c where

  -- | A blank lookup table.
  empty :: c a

  -- | Insert the provided value into the table
  insert :: a -> c a -> (Key, c a)

  -- | Remove the specified entry from the table.  Won't do anything
  --   if the key is not in the table.
  delete :: Key -> c a -> c a

  -- | Return the value associated with the specified key if it is in
  --   the table.
  lookup :: Key -> c a -> (Maybe a)

  -- | As with 'lookup', but assumes the key is in the table.
  unsafeLookup   :: Key -> c a -> a
  unsafeLookup k = fromMaybe err . lookup k
    where
      err = error $ "The key `" ++ show k ++ "' does not have a corresponding value."

  -- | Is the following entry in the table?
  hasEntry   :: Key -> c a -> Bool
  hasEntry k = isJust . lookup k

  -- | Apply a function on the value associated with the specified key.
  adjust :: (a -> a) -> Key -> c a -> (c a)

  -- | How many values are currently stored.
  size :: c a -> Int
  size = length . keys

  -- | The smallest key being used; 'Nothing' indicates 'isNull'.
  minKey :: c a -> Maybe Key

  -- | The largest key being used; 'Nothing' indicates 'isNull'.  Note
  --   that the next key will /not/ necessarily be @(+1)@ of this.
  maxKey :: c a -> Maybe Key

  -- | Are there any values being stored?
  isNull :: c a -> Bool
  isNull = (0==) . size

  -- | Return all keys in the table.
  keys :: c a -> [Key]
  keys = fmap fst . assocs

  -- | Return all values in the table.
  values :: c a -> [a]
  values = fmap snd . assocs

  -- | Return all @(key,value)@ pairs in the table.
  assocs :: c a -> [(Key, a)]

  -- | Create a table from a list of specified values.  The value at
  --   @xs !! k@ will be associated with the key @k@ in @fromList xs@.
  fromList :: [a] -> c a
  fromList = unsafeFromListWithKeys . zip [initKey..]

  -- | Create a table from the specified @(key,value)@ pairs (this is
  --   the inverse of @assocs@).
  --
  --   The keys are /not/ assumed to be in any particular order.
  --
  --   However, the behaviour is undefined if any keys are @<0@ or
  --   there are duplicate keys.
  unsafeFromListWithKeys :: [(Key, a)] -> c a

  -- | Merge the two tables together by \"appending\" the second table
  --   to the first.  Also returned is the translation function
  --   between keys from the second table to the first.
  merge :: c a -> c a -> ((Key -> Key), c a)

  -- | Remove any keys present in the second table from the first.
  difference :: c a -> c a -> c a

  -- | Apply a mapping function over the values in the table whilst
  --   also considering the actual keys.
  mapWithKey :: (Key -> a -> b) -> c a -> c b

-- Need equivalent of M.unions for non-overlapping tables?

-- Traversal and face-finding code currently takes advantage of using
-- Map to do difference; better way of doing it?

-- | An alias for 'fmap'.
map :: (Keyless c) => (a -> b) -> c a -> c b
map = fmap

-- | A flipped, infix alias for 'unsafeLookup'.
(!) :: (Keyless c) => c a -> Key -> a
c ! k = unsafeLookup k c

-- | A flipped, infix alias for 'lookup'.
(!?) :: (Keyless c) => c a -> Key -> Maybe a
c !? a = lookup a c
