module Data.BinaryTrie where

-- binary trie for huffman decoding
data Trie a = Node (Trie a) (Trie a) | Leaf (Maybe a)

empty :: Trie a
empty = Leaf Nothing

insert :: [Bool] -> a -> Trie a -> Trie a
insert [] x _ = Leaf (Just x)
insert ks x (Leaf _) = go ks x where
  go [] x = Leaf (Just x)
  go (False:ks) x = Node (go ks x) (Leaf Nothing)
  go (True :ks) x = Node (Leaf Nothing) (go ks x)
insert (False:ks) x (Node l r) = Node (insert ks x l) r
insert (True :ks) x (Node l r) = Node l (insert ks x r)

fromList :: [([Bool], a)] -> Trie a
fromList = foldl (flip . uncurry $ insert) empty

subTrie :: Bool -> Trie a -> Trie a
subTrie k (Node l r) = case k of
  False -> l
  True  -> r
