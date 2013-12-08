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
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text)
import Data.Text.Binary ()
import qualified Data.Map as M
import qualified Data.HashMap.Lazy as H


data Identifiers = Identifiers { ids   :: !(Map     Text Int)   -- Using Map for size O(1)
                               , names :: !(HashMap Int Text)
                               } deriving Eq

instance Show Identifiers where
    show s = "insertMany empty " ++ show (M.keys (ids s))

instance Binary Identifiers where
    put s = put (M.toList $ ids s) >> put (H.toList $ names s)
    get = Identifiers <$> (M.fromList <$> get) <*> (H.fromList <$> get)

-- | The empty Identifiers
empty :: Identifiers
empty = Identifiers M.empty H.empty

-- | New Identifiers from list
fromList :: [Text] -> Identifiers
fromList = insertMany empty

-- | Insert item into set (given it a new id)
insert :: Identifiers -> Text -> Identifiers
insert (Identifiers !ids' !names') !v =
    let x = M.findWithDefault next v ids'
        next = M.size ids' + 1
    in Identifiers (M.insert v x ids') (H.insert x v names')

-- | Insert many items into set
insertMany :: Identifiers -> [Text] -> Identifiers
insertMany = foldl' insert

-- | Size of id-set
size :: Identifiers -> Int
size = M.size . ids

-- | New List from Identifiers
toList :: Identifiers -> [Text]
toList = M.keys . ids

-- | Find id for given key
lookupId :: Identifiers -> Text -> Maybe Int
lookupId = flip M.lookup . ids

unsafeLookupId :: Identifiers -> Text -> Int
unsafeLookupId = (M.!) . ids

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

