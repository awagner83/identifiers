module Data.TrieMap where

import Control.DeepSeq
import Data.List (foldl')
import Prelude hiding (lookup)


data TrieMap k v = Map !(Maybe v) [TrieNode k v] deriving (Show, Eq)
data TrieNode k v = EmptyNode !k    [TrieNode k v]
                  | ValueNode !k !v [TrieNode k v]
                  deriving (Show, Eq)

instance (NFData k, NFData v) => NFData (TrieMap k v) where
    rnf (Map v ns) = rnf (v, ns)

instance (NFData k, NFData v) => NFData (TrieNode k v) where
    rnf (EmptyNode k ns)   = rnf (k, ns)
    rnf (ValueNode k v ns) = rnf (k, v, ns)


-- | The empty TrieMap
empty :: TrieMap k v
empty = Map Nothing []

-- | Create map from list of associations
fromList :: Eq k => [([k], v)] -> TrieMap k v
fromList = foldl' go empty
    where go m (ks, v) = insert m ks v

-- | Search for a value in the map
lookup :: Eq k => TrieMap k v -> [k] -> Maybe v
lookup (Map Nothing []) _  = Nothing
lookup (Map v _)        [] = v
lookup (Map _ ns)       ks = go ns ks where
    go []                       _                        = Nothing
    go _                        []                       = Nothing
    go (EmptyNode j next:ns')   ks'@(k:ks'') | j == k    = go next ks''
                                             | otherwise = go ns' ks'
    go (ValueNode j v _:ns')    ks'@[k]      | j == k    = Just v
                                             | otherwise = go ns' ks'
    go (ValueNode j _ next:ns') ks'@(k:ks'') | j == k    = go next ks''
                                             | otherwise = go ns' ks'

(!) :: Eq k => TrieMap k v -> [k] -> v
m ! k | Just v <- lookup m k = v
      | otherwise = error "oh noes!"

-- | Insert new word into map
insert :: Eq k => TrieMap k v -> [k] -> v -> TrieMap k v
insert (Map Nothing ns) [] v  = Map (Just v) ns
insert m                [] _  = m       -- Don't clobber existing values
insert (Map v ns)       ks v' = Map v $ go ns ks where
    go _  []        = []    -- Not sure how we got here; try to handle anyway.
    go [] (x:[])    = [ValueNode x v' []]
    go [] (x:xs)    = [EmptyNode x (go [] xs)]

    -- Last key unit vs ValueNode
    go ns''@(n@(ValueNode j _ _):ns') xs'@(x:[])
        | j == x    = ns''                          -- No clobber
        | otherwise = n : go ns' xs'

    -- Last key unit vs EmptyNode
    go (n@(EmptyNode j next):ns') xs'@(x:[])
        | j == x    = ValueNode j v' next : ns'     -- Promote to ValueNode
        | otherwise = n : go ns' xs'

    -- Key unit vs ValueNode
    go (n@(ValueNode j w next):ns') xs'@(x:xs)
        | j == x    = ValueNode j w (go next xs) : ns'  -- Decend into node
        | otherwise = n : go ns' xs'

    -- Key unit vs EmptyNode
    go (n@(EmptyNode j next):ns') xs'@(x:xs)
        | j == x    = EmptyNode j (go next xs) : ns'    -- Decend into node
        | otherwise = n : go ns' xs'

{-
insert []                (k:[]) v           = [R k (Just v) []]
insert []                (k:ks) v           = [R k Nothing (insert [] ks v)]
insert (R j _ next : rs) (k:[]) v' | j == k = R j (Just v') next : rs
insert (R j v next : rs) (k:ks) v' | j == k = R j v (insert next ks v') : rs
insert xs                []     _           = xs
insert (r:rows)          ks     v           = r : insert rows ks v
-}

