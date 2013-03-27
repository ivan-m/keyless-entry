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

  -- | A blank lookup table with specified default size.  Only useful
  --   for 'Vector'-based tables; otherwise identical to 'empty'.
  emptySized   :: Int -> c a
  emptySized _ = empty

  -- | Insert the provided value into the table
  insert :: a -> c a -> (Key, c a)

  -- | Add multiple values into the table.
  insertBulk :: [a] -> c a -> ([Key], c a)

  -- | Remove the specified entry from the table.  Won't do anything
  --   if the key is not in the table.
  delete :: Key -> c a -> c a

  -- | Remove multiple entries from the table.
  deleteBulk :: [Key] -> c a -> c a

  -- | Return the value associated with the specified key if it is in
  --   the table.
  lookup :: Key -> c a -> (Maybe a)

  -- | As with 'lookup', but assumes the key is in the table.
  unsafeLookup   :: Key -> c a -> a

  -- | Is the following entry in the table?
  hasEntry   :: Key -> c a -> Bool

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

  -- | Return all keys in the table.
  keys :: c a -> [Key]

  -- | Return all values in the table.
  values :: c a -> [a]

  -- | Return all @(key,value)@ pairs in the table.
  assocs :: c a -> [(Key, a)]

  -- | Create a table from a list of specified values.  The value at
  --   @xs !! k@ will be associated with the key @k@ in @fromList xs@.
  fromList :: [a] -> c a

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

  -- | Given a non-empty list of tables, merge them all together in
  --   order and provide the translation function for each (the first
  --   function will be @'id'@).
  --
  --   An empty list will return the 'empty' table.
  mergeAll :: [c a] -> ([MergeTranslation Key], c a)

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


-- | When merging multiple 'Keyless' tables together, to be able to
--   translate values from each original table to the new values and
--   positions several pieces of information are needed:
--
--   * The keys in the new table that corresponds to the original;
--     this is the keys bounded by 'newBounds' inclusive.  Bounds of
--     'Nothing' denote that the original table was 'null'.
--
--   * The /partial/ function in 'oldToNew' translates keys from the
--     old table to keys in the new table.
data MergeTranslation k = MT { -- | The portion of the new table from
                               --   the applicable old table.
                               newBounds :: !(Maybe (Key,Key))
                               -- | The translation function for keys
                               --   from the original table to the new
                               --   one.
                             , oldToNew  :: !(k -> k)
                             }
