{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DefaultSignatures,
             FlexibleContexts, MultiParamTypeClasses, TypeFamilies,
             TypeOperators #-}

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

class Keyless c where

  -- | The type of values stored in this keyless table.
  type Elem c :: *

  -- | Additional arguments used when building a keyless table.
  --   Defaults to @'[]@.
  type BuildArgs c :: [*]
  type BuildArgs c = '[]

  -- | A blank lookup table.
  empty :: BuildArguments c c

  -- | Insert the provided value into the table
  insert :: Elem c -> c -> (Key, c)

  -- | Add multiple values into the table.
  insertBulk :: [Elem c] -> c -> ([Key], c)

  -- | Remove the specified entry from the table.  Won't do anything
  --   if the key is not in the table.
  delete :: Key -> c -> c

  -- | Remove multiple entries from the table.
  deleteBulk :: [Key] -> c -> c

  -- | Return the value associated with the specified key if it is in
  --   the table.
  lookup :: Key -> c -> Maybe (Elem c)

  -- | As with 'lookup', but assumes the key is in the table.
  unsafeLookup :: Key -> c -> Elem c

  -- | Is the following entry in the table?
  hasEntry :: Key -> c -> Bool

  -- | Apply a function on the value associated with the specified key.
  adjust :: (Elem c -> Elem c) -> Key -> c -> c

  -- | How many values are currently stored.
  size :: c -> Int
  size = length . keys

  -- | The smallest key being used; 'Nothing' indicates 'isNull'.
  minKey :: c -> Maybe Key

  -- | The largest key being used; 'Nothing' indicates 'isNull'.  Note
  --   that the next key will /not/ necessarily be @(+1)@ of this.
  maxKey :: c -> Maybe Key

  -- | Are there any values being stored?
  isNull :: c -> Bool

  -- | Return all keys in the table.
  keys :: c -> [Key]

  -- | Return all values in the table.
  values :: c -> [Elem c]

  -- | Return all @(key,value)@ pairs in the table.
  assocs :: c -> [(Key, Elem c)]

  -- | Create a table from a list of specified values.  The value at
  --   @xs !! k@ will be associated with the key @k@ in @fromList xs@.
  fromList :: BuildArguments c ([Elem c] -> c)

  -- | Create a table from the specified @(key,value)@ pairs (this is
  --   the inverse of @assocs@).
  --
  --   The keys are /not/ assumed to be in any particular order
  --   (i.e. they will probably be sorted first, which will thus
  --   affect performance).
  unsafeFromListWithKeys :: BuildArguments c ([(Key, Elem c)] -> c)

  -- | Merge the two tables together by \"appending\" the second table
  --   to the first.  Also returned is the translation function
  --   between keys from the second table to the first.
  merge :: c -> c -> (Key -> Key, c)

  -- | Given a non-empty list of tables, merge them all together in
  --   order and provide the translation function for each (the first
  --   function will be @'id'@).
  --
  --   An empty list will return the 'empty' table.
  mergeAll :: [c] -> ([MergeTranslation Key], c)

  -- | Remove any keys present in the second table from the first.
  difference :: c -> c -> c

-- | 'Keyless' tables that can be deconstructed into the container (of
--   kind @* -> *@) and the value being stored.
--
--   The @a@ parameter remains in case of instances with constraints
--   on the values used.
class (Keyless (c a), Elem (c a) ~ a) => FKeyless c a where

  map :: (FKeyless c b) => (a -> b) -> c a -> c b
  default map :: (Functor c) => (a -> b) -> c a -> c b
  map = fmap

  -- | Apply a mapping function over the values in the table whilst
  --   also considering the actual keys.
  mapWithKey :: (FKeyless c b) => (Key -> a -> b) -> c a -> c b

-- Need equivalent of M.unions for non-overlapping tables?

-- Traversal and face-finding code currently takes advantage of using
-- Map to do difference; better way of doing it?

-- | A flipped, infix alias for 'unsafeLookup'.
(!) :: (Keyless c) => c -> Key -> Elem c
c ! k = unsafeLookup k c

-- | A flipped, infix alias for 'lookup'.
(!?) :: (Keyless c) => c -> Key -> Maybe (Elem c)
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
                               newBounds :: !(Maybe (k,k))
                               -- | The translation function for keys
                               --   from the original table to the new
                               --   one.
                             , oldToNew  :: !(k -> k)
                             }

-- | Expand a list of extra arguments out as function parameters.
--
--   For example, @ApplyOperands '[Int, Char] Bool ~ Int -> Char -> Bool@.
type family ApplyOperands (os :: [*]) r where
  ApplyOperands '[] r = r
  ApplyOperands (o ': os) r = o -> ApplyOperands os r

-- | The arguments needed to create a 'Keyless' value @c@ with a
--   result type of @r@.
type BuildArguments c r = ApplyOperands (BuildArgs c) r
