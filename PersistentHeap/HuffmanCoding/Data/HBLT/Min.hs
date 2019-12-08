module Data.HBLT.Min 
  ( Heap
  , Data.HBLT.Min.null
  , empty
  , singleton
  , merge
  , insert
  , view
  , viewHead
  , viewTail
  , fromList
  )
  where

data Heap a = Node {-# UNPACK #-} !Int !a (Heap a) (Heap a) | Nil
  deriving Show

null :: Heap a -> Bool
null Nil = True
null _   = False

empty :: Heap a
empty = Nil

singleton :: a -> Heap a
singleton x = Node 1 x Nil Nil

merge :: Ord a => Heap a -> Heap a -> Heap a
merge Nil h = h
merge h Nil = h
merge h1 h2
  | k1 <= k2 =
    let a = l1
        b = merge r1 h2
        sa = svalue a
        sb = svalue b
    in if sa >= sb
      then Node (sb+1) k1 a b
      else Node (sa+1) k1 b a
  | otherwise =
    let a = l2
        b = merge r2 h1
        sa = svalue a
        sb = svalue b
    in if sa >= sb
      then Node (sb+1) k2 a b
      else Node (sa+1) k2 b a
  where
    Node s1 k1 l1 r1 = h1
    Node s2 k2 l2 r2 = h2

    svalue (Node s _ _ _) = s
    svalue Nil = 0

insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge h (singleton x)

view :: Ord a => Heap a -> Maybe (a, Heap a)
view Nil = Nothing
view (Node _ x l r) = Just (x, merge l r)

viewHead :: Heap a -> Maybe a
viewHead Nil = Nothing
viewHead (Node _ x _ _) = Just x

viewTail :: Ord a => Heap a -> Maybe (Heap a)
viewTail Nil = Nothing
viewTail (Node _ x l r) = Just (merge l r)

fromList :: Ord a => [a] -> Heap a
fromList xs = fold (go xs) [] where
  go [] = []
  go [x] = [singleton x]
  go (x:y:xs) = if x <= y
    then Node 1 x (Node 1 y Nil Nil) Nil : go xs
    else Node 1 y (Node 1 x Nil Nil) Nil : go xs
  
  fold [] []  = empty
  fold [] [y] = y
  fold [] ys  = fold ys []
  fold [x] ys = fold [] (x:ys)
  fold (x:y:xs) ys = fold xs (merge x y : ys)
