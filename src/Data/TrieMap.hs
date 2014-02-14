module Data.TrieMap where

import Control.DeepSeq
import Data.List (foldl')
import Prelude hiding (lookup)

type TrieMap k v = [Row k v]
data Row k v = R !k !(Maybe v) !(TrieMap k v) deriving Show

instance (NFData k, NFData v) => NFData (Row k v) where
    rnf (R a b c) = rnf (a, b, c)

-- | The empty TrieMap
empty :: TrieMap k v
empty = []

-- | Create map from list of associations
fromList :: Eq k => [([k], v)] -> TrieMap k v
fromList = foldl' go empty
    where go m (ks, v) = insert m ks v

-- | Search for a value in the map
lookup :: Eq k => TrieMap k v -> [k] -> Maybe v
lookup [] _ = Nothing
lookup _ [] = Nothing
lookup (R j v _ : rs) [k]           | j == k    = v
                                    | otherwise = lookup rs [k]
lookup (R j _ next : rs) ks'@(k:ks) | j == k    = lookup next ks
                                    | otherwise = lookup rs ks'

(!) :: Eq k => TrieMap k v -> [k] -> v
m ! k | Just v <- lookup m k = v
      | otherwise = error "oh noes!"

-- | Insert new word into map
insert :: Eq k => TrieMap k v -> [k] -> v -> TrieMap k v
insert []                (k:[]) v           = [R k (Just v) []]
insert []                (k:ks) v           = [R k Nothing (insert [] ks v)]
insert (R j _ next : rs) (k:[]) v' | j == k = R j (Just v') next : rs
insert (R j v next : rs) (k:ks) v' | j == k = R j v (insert next ks v') : rs
insert xs                []     _           = xs
insert (r:rows)          ks     v           = r : insert rows ks v

