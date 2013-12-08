{-# LANGUAGE BangPatterns #-}

module Data.Text.Identifiers
    ( Identifiers ()
    
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
import Data.HashMap.Lazy (HashMap)
import Data.Maybe
import Data.Text (Text)
import Data.Text.Binary ()
import qualified Data.HashMap.Lazy as H


data Identifiers = Identifiers { ids   :: !(HashMap Text Int)
                               , names :: !(HashMap Int Text)
                               , size  :: Int
                               } deriving Eq

instance Show Identifiers where
    show s = "insertMany empty " ++ show (H.keys (ids s))

instance Binary Identifiers where
    put s = put (H.toList $ ids s) >> put (H.toList $ names s) >> put (size s)
    get = Identifiers <$> (H.fromList <$> get) <*> (H.fromList <$> get) <*> get

-- | The empty Identifiers
empty :: Identifiers
empty = Identifiers H.empty H.empty 0

-- | New Identifiers from list
fromList :: [Text] -> Identifiers
fromList = insertMany empty

-- | Insert item into set (given it a new id)
insert :: Identifiers -> Text -> Identifiers
insert xs v = case H.lookup v (ids xs) of
        Just _  -> xs
        Nothing -> Identifiers (H.insert v next $ ids xs)
                               (H.insert next v $ names xs)
                               next
    where next = size xs + 1

-- | Insert many items into set
insertMany :: Identifiers -> [Text] -> Identifiers
insertMany = foldl' insert

-- | New List from Identifiers
toList :: Identifiers -> [Text]
toList = H.keys . ids

-- | Find id for given key
lookupId :: Identifiers -> Text -> Maybe Int
lookupId = flip H.lookup . ids

unsafeLookupId :: Identifiers -> Text -> Int
unsafeLookupId = (H.!) . ids

-- | Find key for given id
lookupKey :: Identifiers -> Int -> Maybe Text
lookupKey = flip H.lookup . names

-- | Given many ids, return many keys
lookupKeys :: Identifiers -> [Int] -> [Text]
lookupKeys s = mapMaybe (lookupKey s)

unsafeLookupKey :: Identifiers -> Int -> Text
unsafeLookupKey = (H.!) . names

(!) :: Identifiers -> Int -> Text
(!) = unsafeLookupKey

-- | Items inserted are given ids
prop_hasId :: Text -> Bool
prop_hasId x = isJust . lookupId (insert empty x) $ x

-- | Inserted items have stable ids
prop_stableId :: Text -> Bool
prop_stableId x = isJust a && a == b
    where a = lookupId firstSet x
          b = lookupId secondSet x
          firstSet = insert empty x
          secondSet = insert firstSet x

-- | Given id can be used to fetch inserted item
prop_keyRetrieval :: [Text] -> Bool
prop_keyRetrieval xs = all (\x -> ret x == x) xs
    where ret = unsafeLookupKey s . unsafeLookupId s
          s = insertMany empty xs

-- | Inserting something more than once does not change the set
prop_idempotent :: Text -> Bool
prop_idempotent x = insert empty x == insert (insert empty x) x

