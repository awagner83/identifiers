module Data.Identifiers
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
    , prop_keyRetrievalUnsafe
    , prop_idempotent

    ) where

import Control.Applicative hiding (empty)
import Control.DeepSeq
import Data.Binary
import Data.List (foldl')
import Data.Hashable
import Data.HashMap.Lazy (HashMap)
import Data.Maybe
import Data.Sequence (Seq, (|>))
import qualified Data.HashMap.Lazy as H
import qualified Data.Sequence as S


data Identifiers a = Identifiers { ids   :: !(HashMap a Int)
                                 , names :: !(Seq a)
                                 } deriving Eq

instance Show a => Show (Identifiers a) where
    show s = "insertMany empty " ++ show (H.keys (ids s))

instance (Eq a, Hashable a, Binary a) => Binary (Identifiers a) where
    put s = put (H.toList $ ids s) >> put (names s)
    get = Identifiers <$> (H.fromList <$> get) <*> get

instance NFData a => NFData (Identifiers a) where
    rnf (Identifiers i n) = rnf (i, n)

-- | The empty Identifiers
empty :: Identifiers a
empty = Identifiers H.empty S.empty

-- | New Identifiers from list
fromList :: (Hashable a, Eq a) => [a] -> Identifiers a
fromList = insertMany empty

-- | Insert item into set (given it a new id)
insert :: (Hashable a, Eq a) => Identifiers a -> a -> Identifiers a
insert xs v = case H.lookup v (ids xs) of
        Just _  -> xs
        Nothing -> Identifiers (H.insert v next $ ids xs) (names xs |> v)
    where next = S.length (names xs)

-- | Insert many items into set
insertMany :: (Hashable a, Eq a) => Identifiers a -> [a] -> Identifiers a
insertMany = foldl' insert

-- | New List from Identifiers
toList :: Identifiers a -> [a]
toList = H.keys . ids

-- | Find id for given key
lookupId :: (Hashable a, Eq a) => Identifiers a -> a -> Maybe Int
lookupId = flip H.lookup . ids

size :: Identifiers a -> Int
size = S.length . names

unsafeLookupId :: (Hashable a, Eq a) => Identifiers a -> a -> Int
unsafeLookupId = (H.!) . ids

-- | Find key for given id
lookupKey :: Identifiers a -> Int -> Maybe a
lookupKey ident x = let xs = names ident
                    in if S.length xs < x
                       then Nothing
                       else Just $ unsafeLookupKey ident x

-- | Given many ids, return many keys
lookupKeys :: Identifiers a -> [Int] -> [a]
lookupKeys s = mapMaybe (lookupKey s)

unsafeLookupKey :: Identifiers a -> Int -> a
unsafeLookupKey = S.index . names

(!) :: Identifiers a -> Int -> a
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
prop_keyRetrievalUnsafe :: [String] -> Bool
prop_keyRetrievalUnsafe xs = all (\x -> ret x == x) xs
    where ret = unsafeLookupKey s . unsafeLookupId s
          s = insertMany empty xs

-- | Given id can be used to fetch inserted item
prop_keyRetrieval :: [String] -> Bool
prop_keyRetrieval xs = all (\x -> ret x == Just (Just x)) xs
    where ret x = lookupKey s <$> lookupId s x
          s = insertMany empty xs

-- | Inserting something more than once does not change the set
prop_idempotent :: String -> Bool
prop_idempotent x = insert empty x == insert (insert empty x) x

