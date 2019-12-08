module Main where

import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.HBLT.Min as H
import qualified Data.BinaryTrie as T
import Data.Tuple

data HTree a = Node {-# UNPACK #-} !Int (HTree a) (HTree a)
             | Leaf a Int
             deriving Show

freq :: HTree a -> Int
freq (Leaf _ n) = n
freq (Node n _ _) = n

instance Eq (HTree a) where
  x == y = freq x == freq y

instance Ord (HTree a) where
  x < y = freq x < freq y
  x <= y = freq x <= freq y
  compare x y = compare (freq x) (freq y)

makeNode :: HTree a -> HTree a -> HTree a
makeNode x y = Node (freq x + freq y) x y

makeTree :: [(a,Int)] -> HTree a
makeTree xs = go . H.fromList . map (uncurry Leaf) $ xs where
  go h = case H.view h of
    Just (x,h') -> case H.view h' of
      Just (y,h'') -> go $ H.insert (makeNode x y) h''
      Nothing -> x
    Nothing -> error "makeTree : empty input"

makeFreqTable :: Ord a => [a] -> M.Map a Int
makeFreqTable xs = go M.empty xs where
  go m [] = m
  go m (x:xs) = go (M.alter f x m) xs
  f Nothing  = Just 1
  f (Just n) = Just (n+1)

makeTableFromTree :: Ord a => HTree a -> M.Map a [Bool]
makeTableFromTree t = M.fromList $ go [] t [] where
  go p (Node _ l r) = go (False:p) l . go (True:p) r
  go p (Leaf x _) = ((x,reverse p):)

encode :: Ord a => M.Map a [Bool] -> [a] -> [Bool]
encode m xs = concat $ map (m!) xs

decode :: M.Map a [Bool] -> [Bool] -> [a]
decode m bs = go t bs where
  t = T.fromList . map swap $ M.toList m
  go st bs = case st of
    T.Node l r -> case bs of
      []        -> error "failed to find matching"
      False:bs' -> go l bs'
      True :bs' -> go r bs'
    T.Leaf Nothing -> error "failed to find matching"
    T.Leaf (Just x) -> case bs of
      [] -> [x]
      _  -> x : go t bs

showBinary :: [Bool] -> String
showBinary [] = []
showBinary (True:xs)  = '1' : showBinary xs
showBinary (False:xs) = '0' : showBinary xs

main = do
  xs <- getLine
  let table = makeTableFromTree
            . makeTree
            . M.toList
            . makeFreqTable
            $ xs
      bs = encode table xs
  mapM_ (putStrLn . showPair) $ M.toList table
  putStr "encoded : "
  putStrLn $ showBinary bs
  putStr "decoded : "
  putStrLn $ decode table bs
  where
    showPair (x,c) = show x ++ " : " ++ showBinary c
