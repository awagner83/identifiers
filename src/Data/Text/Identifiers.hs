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

