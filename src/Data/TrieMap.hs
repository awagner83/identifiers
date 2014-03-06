module Data.TrieMap where

import Control.DeepSeq
import Data.List (foldl')
import Prelude hiding (lookup)

--                                      Deep           Wide
data TrieMap k v  = Root              !(TrieNode k v)
                  | ValueRoot     !v  !(TrieNode k v)
                  | FlatRoot      !v
                  | EmptyRoot
                  deriving (Show, Eq)

data TrieNode k v = Node        !k    !(TrieNode k v) !(TrieNode k v)
                  | Vertical    !k    !(TrieNode k v)
                  | Horizontal  !k                    !(TrieNode k v)
                  | ValueVert   !k !v !(TrieNode k v)
                  | ValueHoriz  !k !v                 !(TrieNode k v)
                  | ValueNode   !k !v !(TrieNode k v) !(TrieNode k v)
                  | ValueBottom !k !v
                  deriving (Show, Eq)

instance (NFData k, NFData v) => NFData (TrieMap k v) where
    rnf (Root a)        = rnf a
    rnf (ValueRoot a b) = rnf (a, b)
    rnf (FlatRoot a)    = rnf a
    rnf (EmptyRoot)     = rnf ()

instance (NFData k, NFData v) => NFData (TrieNode k v) where
    rnf (Node a b c)        = rnf (a, b, c)
    rnf (Vertical a b)      = rnf (a, b)
    rnf (Horizontal a b)    = rnf (a, b)
    rnf (ValueVert a b c)   = rnf (a, b, c)
    rnf (ValueHoriz a b c)  = rnf (a, b, c)
    rnf (ValueNode a b c d) = rnf (a, b, c, d)
    rnf (ValueBottom a b)   = rnf (a, b)

-- | The empty TrieMap
empty :: TrieMap k v
empty = EmptyRoot

-- | Create map from list of associations
fromList :: Eq k => [([k], v)] -> TrieMap k v
fromList = foldl' go empty
    where go m (ks, v) = insert m ks v

-- | Search for a value in the map
lookup :: Eq k => TrieMap k v -> [k] -> Maybe v
lookup (EmptyRoot)        _  = Nothing
lookup (FlatRoot v)       [] = Just v
lookup (FlatRoot _)       _  = Nothing
lookup (ValueRoot v _)    [] = Just v
lookup (ValueRoot _ next) ks = lookupNode next ks
lookup (Root _)           [] = Nothing
lookup (Root next)        ks = lookupNode next ks

-- | Recursive lookup from nodes down (excluding root)
lookupNode :: Eq k => TrieNode k v -> [k] -> Maybe v
lookupNode _        [] = Nothing
lookupNode (Node k down right) ks@(x:xs)
    | x == k    = lookupNode down  xs
    | otherwise = lookupNode right ks
lookupNode (ValueNode k v down right) ks@(x:xs)
    | null xs && x == k = Just v
    |            x == k = lookupNode down  xs
    | otherwise         = lookupNode right ks
lookupNode (ValueBottom k v) (x:xs)
    | null xs && x == k = Just v
    | otherwise         = Nothing
lookupNode (Vertical k down) (x:xs)
    | null xs || x /= k = Nothing
    | otherwise         = lookupNode down xs
lookupNode (Horizontal k right) ks@(x:_)
    | x /= k            = Nothing
    | otherwise         = lookupNode right ks
lookupNode (ValueVert k v down) (x:xs)
    | null xs && x == k = Just v
    |            x == k = lookupNode down xs
    | otherwise         = Nothing
lookupNode (ValueHoriz k v right) ks@(x:xs)
    | null xs && x == k = Just v
    |            x == k = Nothing
    | otherwise         = lookupNode right ks

-- | Unsafe indexing into TrieMap
(!) :: Eq k => TrieMap k v -> [k] -> v
m ! k | Just v <- lookup m k = v
      | otherwise = error "Key not found in TrieMap"

-- | Insert new word into map
insert :: Eq k => TrieMap k v -> [k] -> v -> TrieMap k v
insert (Root down)        [] v = ValueRoot v down
insert (ValueRoot _ down) [] v = ValueRoot v down
insert (FlatRoot _)       [] v = FlatRoot v
insert EmptyRoot          [] v = FlatRoot v
insert (Root down)        ks v = Root        $ insertNode down ks v
insert (ValueRoot w down) ks v = ValueRoot w $ insertNode down ks v
insert (FlatRoot w)       ks v = ValueRoot w $ createNode ks v
insert EmptyRoot          ks v = Root        $ createNode ks v

-- | Insert value into existing tree of nodes
insertNode :: Eq k => TrieNode k v -> [k] -> v -> TrieNode k v
insertNode _ [] _ = error "insertNode should never be called with an empty key"
insertNode (Node k down right) ks@(x:xs) v
    | null xs && x == k = ValueNode k v down right
    |            x == k = Node k (insertNode down xs v) right
    | otherwise         = Node k down $ insertNode right ks v
insertNode (Vertical k down) ks@(x:xs) v 
    | null xs && x == k = ValueVert k v down
    |            x == k = Vertical k $ insertNode down xs v
    | otherwise         = Node k down $ createNode ks v
insertNode (Horizontal k right) ks@(x:xs) v
    | null xs && x == k = ValueHoriz k v right
    |            x == k = Node k (createNode xs v) right
    | otherwise         = Horizontal k $ insertNode right ks v
insertNode (ValueVert k w down) ks@(x:xs) v
    | null xs && x == k = ValueVert k v down
    |            x == k = ValueVert k w $ insertNode down xs v
    | otherwise         = ValueNode k w down $ createNode ks v
insertNode (ValueHoriz k w right) ks@(x:xs) v
    | null xs && x == k = ValueHoriz k v right
    |            x == k = ValueNode k w (createNode xs v) right
    | otherwise         = ValueHoriz k w $ insertNode right ks v
insertNode (ValueNode k w down right) ks@(x:xs) v
    | null xs && x == k = ValueNode k v down right
    |            x == k = ValueNode k w (insertNode down xs v) right
    | otherwise         = ValueNode k w down (insertNode right ks v)
insertNode (ValueBottom k w) ks@(x:xs) v
    | null xs && x == k = ValueBottom k v
    |            x == k = ValueVert k w $ createNode xs v
    | otherwise         = ValueHoriz k w $ createNode ks v

-- | Create node (or series of nodes) to point to value
createNode :: Eq k => [k] -> v -> TrieNode k v
createNode [] _ = error "createNode should never be called with an empty key"
createNode (k:[]) v = ValueBottom k v
createNode (k:ks) v = Vertical k $ createNode ks v

