{-# LANGUAGE ViewPatterns #-}
module Data.Identifiers.ListLike
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

import Control.Arrow ((&&&))
import Control.Applicative hiding (empty)
import Control.DeepSeq
import Data.Binary
import Data.List (foldl', isPrefixOf)
import Data.Map (Map)
import Data.Maybe
import Data.Sequence (Seq, (|>))
import Data.Serialize (Serialize)
import Data.ListLike (ListLike)
import Data.TrieMap (TrieMap)
import qualified Data.Map as M
import qualified Data.TrieMap as TM
import qualified Data.Sequence as S
import qualified Data.Serialize as C
import qualified Data.Foldable as F
import qualified Data.ListLike as LL


data Identifiers i n u = Identifiers { ids   :: !(TrieMap u i)
                                     , names :: !(Seq n)
                                     } deriving Eq

instance (Show n) => Show (Identifiers i n u) where
    show s = "insertMany empty " ++ show (F.toList (names s))

instance (Binary n, ListLike n u, Integral i, Eq u)
    => Binary (Identifiers i n u) where
    put = put . toList
    get = fromList <$> get

instance (Serialize n, ListLike n u, Integral i, Eq u)
    => Serialize (Identifiers i n u) where
    put = C.put . toList
    get = fromList <$> C.get
 
instance (NFData i, NFData n, NFData u) => NFData (Identifiers i n u) where
    rnf (Identifiers i n) = rnf (i, n)

-- | The empty Identifiers
empty :: Identifiers i n u
empty = Identifiers TM.empty S.empty

-- | New Identifiers from list
fromList :: (ListLike n u, Eq u, Integral i) => [n] -> Identifiers i n u
fromList = insertMany empty

-- | Combine two identifier sets into one.
--   Because the ids will change while combining two sets, a map is also
--   returned that identifies the new location of old ids for the second
--   set passed in.
combine
    :: (ListLike n u, Integral i, Eq u)
    => Identifiers i n u -> Identifiers i n u -> (Identifiers i n u, Map i i)
combine a b = let c  = (insertMany a) xs
                  xs = toList b
                  m  = M.fromList $ map (unsafeLookupId b &&& unsafeLookupId c) xs
              in (c, m)

-- | Insert item into set (given it a new id)
insert :: (ListLike n u, Eq u, Integral i)
    => Identifiers i n u -> n -> Identifiers i n u
insert xs v@(LL.toList -> v') = case TM.lookup (ids xs) v' of
        Just _  -> xs
        Nothing -> Identifiers (TM.insert (ids xs) v' next) (names xs |> v)
    where next = fromIntegral $ S.length $ names xs

-- | Insert many items into set
insertMany :: (ListLike n u, Eq u, Integral i)
    => Identifiers i n u -> [n] -> Identifiers i n u
insertMany = foldl' insert

-- | New List from Identifiers
toList :: Identifiers i n u -> [n]
toList = F.toList . names

-- | Find id for given key
lookupId :: (Eq u, ListLike n u) => Identifiers i n u -> n -> Maybe i
lookupId (ids -> m) (LL.toList -> k) = TM.lookup m k

size :: Identifiers i n u -> Int
size = S.length . names

unsafeLookupId :: (ListLike n u, Eq u) => Identifiers i n u -> n -> i
unsafeLookupId (ids -> m) (LL.toList -> k) = m TM.! k

-- | Find key for given id
lookupKey :: (Integral i) => Identifiers i n u -> i -> Maybe n
lookupKey ident x = let xs = names ident
                    in if S.length xs < fromIntegral x
                       then Nothing
                       else Just $ unsafeLookupKey ident x

-- | Given many ids, return many keys
lookupKeys :: (Integral i) => Identifiers i n u -> [i] -> [n]
lookupKeys s = mapMaybe (lookupKey s)

unsafeLookupKey :: Integral i => Identifiers i n u -> i -> n
unsafeLookupKey xs x = S.index (names xs) (fromIntegral x)

(!) :: Integral i => Identifiers i n u -> i -> n
(!) = unsafeLookupKey

-- | Items inserted are given ids
prop_hasId :: String -> Bool
prop_hasId x = isJust . lookupId (insert (empty :: Identifiers Int String Char) x) $ x

-- | Inserted items have stable ids
prop_stableId :: String -> Bool
prop_stableId x = isJust a && a == b
    where a = lookupId firstSet x
          b = lookupId secondSet x
          firstSet = insert (empty :: Identifiers Int String Char) x
          secondSet = insert firstSet x

-- | Given id can be used to fetch inserted item
prop_keyRetrievalUnsafe :: [String] -> Bool
prop_keyRetrievalUnsafe xs = all (\x -> ret x == x) xs
    where ret = unsafeLookupKey s . unsafeLookupId s
          s = insertMany (empty :: Identifiers Int String Char) xs

-- | Given id can be used to fetch inserted item
prop_keyRetrieval :: [String] -> Bool
prop_keyRetrieval xs = all (\x -> ret x == Just (Just x)) xs
    where ret x = lookupKey s <$> lookupId s x
          s = insertMany (empty :: Identifiers Int String Char) xs

-- | Inserting something more than once does not change the set
prop_idempotent :: String -> Bool
prop_idempotent x = insert (empty :: Identifiers Int String Char) x
                        == insert (insert empty x) x

-- | Ids for the first set passed to combine remain unchanged
prop_stableCombine :: [String] -> [String] -> Bool
prop_stableCombine (fromList -> xs) (fromList -> ys) =
    let (zs, _) = combine xs (ys :: Identifiers Int String Char)
    in (toList xs) `isPrefixOf` (toList zs)

-- | Ensure the migration points to the same value in both old and new sets
prop_properMigration :: [String] -> [String] -> Bool
prop_properMigration (fromList -> xs) (fromList -> ys) =
    let (zs, m) = combine xs (ys :: Identifiers Int String Char)
    in and [ (unsafeLookupKey ys k) == (unsafeLookupKey zs v)
             | (k, v) <- M.toList m ]

