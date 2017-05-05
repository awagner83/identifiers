{-|
Module      : Data.Identifiers.Hashable
Description : Identifiers for Hashable values
Copyright   : (c) Adam Wagner, 2017

Identifiers for Hashable values.

Example usage:

>>> xs = fromList ["foo", "bar", "baz", "foo"]
>>> lookupId xs "baz"
Just 2
>>> lookupKey xs 2
Just "baz"

-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Identifiers.Hashable
    ( Identifiers ()
    
    -- * Construction
    , empty
    , fromList
    , combine
    
    -- * Insertion
    , insert
    , insertMany

    -- * Info
    , size
    
    -- * Extraction
    , toList

    -- * Lookups
    , lookupId
    , lookupKey
    , lookupKeys
    , unsafeLookupId
    , unsafeLookupKey
    , (!)

    -- * Properties
    , prop_hasId
    , prop_stableId
    , prop_keyRetrieval
    , prop_keyRetrievalUnsafe
    , prop_idempotent
    , prop_stableCombine
    , prop_properMigration

    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative hiding (empty)
#endif

import Control.Arrow ((&&&))
import Control.DeepSeq
import Data.Binary
import Data.List (foldl', isPrefixOf)
import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import Data.Map (Map)
import Data.Maybe
import Data.Sequence (Seq, (|>))
import Data.Serialize (Serialize)
import qualified Data.Map as M
import qualified Data.HashMap.Lazy as H
import qualified Data.Sequence as S
import qualified Data.Serialize as C
import qualified Data.Foldable as F


data Identifiers i a = Identifiers { ids   :: !(HashMap a i)
                                   , names :: !(Seq a)
                                   } deriving Eq

instance Show a => Show (Identifiers i a) where
    show s = "insertMany empty " ++ show (toList s)

instance (Binary i, Eq a, Hashable a, Binary a) => Binary (Identifiers i a) where
    put s = put (H.toList $ ids s) >> put (names s)
    get = Identifiers <$> (H.fromList <$> get) <*> get

instance (Serialize i, Eq a, Hashable a, Serialize a) => Serialize (Identifiers i a) where
    put s = C.put (H.toList $ ids s) >> C.put (names s)
    get = Identifiers <$> (H.fromList <$> C.get) <*> C.get

instance (NFData i, NFData a) => NFData (Identifiers i a) where
    rnf (Identifiers i n) = rnf (i, n)

-- | The empty Identifiers
empty :: Identifiers i a
empty = Identifiers H.empty S.empty

-- | New Identifiers from list
fromList :: (Integral i, Hashable a, Eq a) => [a] -> Identifiers i a
fromList = insertMany empty

-- | Combine two identifier sets into one.
--   Because the ids will change while combining two sets, a map is also
--   returned that identifies the new location of old ids for the second
--   set passed in.
combine
    :: (Integral i, Hashable a, Eq a)
    => Identifiers i a -> Identifiers i a -> (Identifiers i a, Map i i)
combine a b = let c  = (insertMany a) xs
                  xs = toList b
                  m  = M.fromList $ map (unsafeLookupId b &&& unsafeLookupId c) xs
              in (c, m)

-- | Insert item into set (given it a new id)
insert :: (Integral i, Hashable a, Eq a) => Identifiers i a -> a -> Identifiers i a
insert xs v = case H.lookup v (ids xs) of
        Just _  -> xs
        Nothing -> Identifiers (H.insert v next $ ids xs) (names xs |> v)
    where next = fromIntegral $ S.length $ names xs

-- | Insert many items into set
insertMany :: (Integral i, Hashable a, Eq a) => Identifiers i a -> [a] -> Identifiers i a
insertMany = foldl' insert

-- | New List from Identifiers
toList :: Identifiers i a -> [a]
toList (names -> xs) = F.toList xs

-- | Find id for given key
lookupId :: (Hashable a, Eq a) => Identifiers i a -> a -> Maybe i
lookupId = flip H.lookup . ids

-- | Number of items in Identifiers value
size :: Identifiers i a -> Int
size = S.length . names

-- | Find numeric id for given value.  Will error when the value is not a member of the Identifiers map.
unsafeLookupId :: (Hashable a, Eq a) => Identifiers i a -> a -> i
unsafeLookupId = (H.!) . ids

-- | Find key for given id
lookupKey :: (Integral i) => Identifiers i a -> i -> Maybe a
lookupKey ident x = let xs = names ident
                    in if S.length xs < fromIntegral x
                       then Nothing
                       else Just $ unsafeLookupKey ident x

-- | Given many ids, return many keys
lookupKeys :: (Integral i) => Identifiers i a -> [i] -> [a]
lookupKeys s = mapMaybe (lookupKey s)

-- | Find id for given value.  Will error when the id has no associated value.
unsafeLookupKey :: Integral i => Identifiers i a -> i -> a
unsafeLookupKey xs x = S.index (names xs) (fromIntegral x)

-- | Infix version of unsafeLookupKey
(!) :: Integral i => Identifiers i a -> i -> a
(!) = unsafeLookupKey

-- | Items inserted are given ids
prop_hasId :: String -> Bool
prop_hasId x = isJust . lookupId (insert (empty :: Identifiers Int String) x) $ x

-- | Inserted items have stable ids
prop_stableId :: String -> Bool
prop_stableId x = isJust a && a == b
    where a = lookupId firstSet x
          b = lookupId secondSet x
          firstSet = insert (empty :: Identifiers Int String) x
          secondSet = insert firstSet x

-- | Given id can be used to fetch inserted item
prop_keyRetrievalUnsafe :: [String] -> Bool
prop_keyRetrievalUnsafe xs = all (\x -> ret x == x) xs
    where ret = unsafeLookupKey s . unsafeLookupId s
          s = insertMany (empty :: Identifiers Int String) xs

-- | Given id can be used to fetch inserted item
prop_keyRetrieval :: [String] -> Bool
prop_keyRetrieval xs = all (\x -> ret x == Just (Just x)) xs
    where ret x = lookupKey s <$> lookupId s x
          s = insertMany (empty :: Identifiers Int String) xs

-- | Inserting something more than once does not change the set
prop_idempotent :: String -> Bool
prop_idempotent x = insert (empty :: Identifiers Int String) x
                        == insert (insert empty x) x

-- | Ids for the first set passed to combine remain unchanged
prop_stableCombine :: [String] -> [String] -> Bool
prop_stableCombine (fromList -> xs) (fromList -> ys) =
    let (zs, _) = combine xs (ys :: Identifiers Int String)
    in (toList xs) `isPrefixOf` (toList zs)

-- | Ensure the migration points to the same value in both old and new sets
prop_properMigration :: [String] -> [String] -> Bool
prop_properMigration (fromList -> xs) (fromList -> ys) =
    let (zs, m) = combine xs (ys :: Identifiers Int String)
    in and [ (unsafeLookupKey ys k) == (unsafeLookupKey zs v)
             | (k, v) <- M.toList m ]

