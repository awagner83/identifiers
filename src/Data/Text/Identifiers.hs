{-# LANGUAGE ConstraintKinds, BangPatterns #-}

module Data.Text.Identifiers
    ( IdSet ()
    , MapKey
    
    -- * Construction
    , empty
    , fromList
    
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
    , prop_idempotent

    ) where

import Control.Applicative hiding (empty)
import Data.Binary
import Data.List (foldl')
import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.HashMap.Lazy as H


data IdSet a = IdSet { ids   :: !(Map     a Int)   -- Using Map for size O(1)
                     , names :: !(HashMap Int a)
                     } deriving Eq

instance (Show a) => Show (IdSet a) where
    show s = "insertMany empty " ++ show (M.keys (ids s))

instance (Binary a, Ord a) => Binary (IdSet a) where
    put s = put (M.toList $ ids s) >> put (H.toList $ names s)
    get = IdSet <$> (M.fromList <$> get) <*> (H.fromList <$> get)

-- | It's best for our keys to be Ord, Hashable, and Eq
type MapKey a = (Ord a, Hashable a, Eq a, Show a)

-- | The empty IdSet
empty :: IdSet a
empty = IdSet M.empty H.empty

-- | New IdSet from list
fromList :: MapKey a => [a] -> IdSet a
fromList = insertMany empty

-- | Insert item into set (given it a new id)
insert :: MapKey a => IdSet a -> a -> IdSet a
insert (IdSet !ids' !names') !v =
    let x = M.findWithDefault next v ids'
        next = M.size ids' + 1
    in IdSet (M.insert v x ids') (H.insert x v names')

-- | Insert many items into set
insertMany :: MapKey a => IdSet a -> [a] -> IdSet a
insertMany = foldl' insert

-- | Size of id-set
size :: IdSet a -> Int
size = M.size . ids

-- | New List from IdSet
toList :: MapKey a => IdSet a -> [a]
toList = M.keys . ids

-- | Find id for given key
lookupId :: (Ord a, Show a) => IdSet a -> a -> Maybe Int
lookupId = flip M.lookup . ids

unsafeLookupId :: Ord a => IdSet a -> a -> Int
unsafeLookupId = (M.!) . ids

-- | Find key for given id
lookupKey :: IdSet a -> Int -> Maybe a
lookupKey = flip H.lookup . names

-- | Given many ids, return many keys
lookupKeys :: IdSet a -> [Int] -> [a]
lookupKeys s = mapMaybe (lookupKey s)

unsafeLookupKey :: IdSet a -> Int -> a
unsafeLookupKey = (H.!) . names

(!) :: IdSet a -> Int -> a
(!) = unsafeLookupKey


-- | Items inserted are given ids
prop_hasId :: String -> Bool
prop_hasId x = isJust . lookupId (insert empty x) $ x

-- | Inserted items have stable ids
prop_stableId :: String -> Bool
prop_stableId x = isJust a && a == b
    where a = lookupId firstSet x
          b = lookupId secondSet x
          firstSet = insert empty x
          secondSet = insert firstSet x

-- | Given id can be used to fetch inserted item
prop_keyRetrieval :: [String] -> Bool
prop_keyRetrieval xs = all (\x -> ret x == x) xs
    where ret = unsafeLookupKey s . unsafeLookupId s
          s = insertMany empty xs

-- | Inserting something more than once does not change the set
prop_idempotent :: String -> Bool
prop_idempotent x = insert empty x == insert (insert empty x) x

