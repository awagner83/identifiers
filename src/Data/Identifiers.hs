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


data Identifiers i a = Identifiers { ids   :: !(HashMap a i)
                                   , names :: !(Seq a)
                                   } deriving Eq

instance Show a => Show (Identifiers i a) where
    show s = "insertMany empty " ++ show (H.keys (ids s))

instance (Binary i, Eq a, Hashable a, Binary a) => Binary (Identifiers i a) where
    put s = put (H.toList $ ids s) >> put (names s)
    get = Identifiers <$> (H.fromList <$> get) <*> get

instance (NFData i, NFData a) => NFData (Identifiers i a) where
    rnf (Identifiers i n) = rnf (i, n)

-- | The empty Identifiers
empty :: Identifiers i a
empty = Identifiers H.empty S.empty

-- | New Identifiers from list
fromList :: (Integral i, Hashable a, Eq a) => [a] -> Identifiers i a
fromList = insertMany empty

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
toList = H.keys . ids

-- | Find id for given key
lookupId :: (Hashable a, Eq a) => Identifiers i a -> a -> Maybe i
lookupId = flip H.lookup . ids

size :: Identifiers i a -> Int
size = S.length . names

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

unsafeLookupKey :: Integral i => Identifiers i a -> i -> a
unsafeLookupKey xs x = S.index (names xs) (fromIntegral x)

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

