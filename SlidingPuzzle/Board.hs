module Board
  (Board, Dir(..), move, fromLL, toLL, manhattan, successors)
  where

import Data.Zipper (Z(..))
import qualified Data.Zipper as Z
import Data.Maybe
import Data.List
import Data.Tree
import Data.Bifunctor

-- Row
data RZ a = RZ {-# UNPACK #-} !Int !(Z a) deriving Show

focusRZL :: RZ a -> RZ a
focusRZL (RZ i z) = RZ (i-1) z

focusRZR :: RZ a -> RZ a
focusRZR (RZ i z) = RZ (i+1) z

fromZ :: Z a -> RZ a
fromZ z = RZ 0 z

toZ :: RZ a -> Z a
toZ (RZ i z) = z' where
  z' = if i < 0
    then iterate Z.focusL z !! (-i)
    else iterate Z.focusR z !! i

maphead :: (a -> a) -> [a] -> [a]
maphead f [] = []
maphead f (x:xs) = f x : xs

unconsRZs :: [RZ a] -> Maybe (Z a, [RZ a])
unconsRZs [] = Nothing
unconsRZs (z@(RZ i _):zs) = Just (toZ z, maphead (focusRZ i) zs) where
  focusRZ x (RZ i z) = RZ (i+x) z

-- (x, y, v)
-- x, y : relative coordinate from correct position
-- right(+), down(+)
-- v    : value
newtype Board = Board {runBoard :: Z (RZ (Int, Int, Int))}

instance Show Board where
  show b = intercalate "\n" (show <$> xss) where
    xss = toLL b

data Dir = L | R | U | D

move :: Dir -> Board -> Maybe (Int, Board)
move dir (Board zz) = case dir of
  L -> case ls of
    [] -> Nothing
    (lx,ly,l):ls' -> Just (dh, Board zz') where
      uzs' = maphead focusRZL uzs
      dzs' = maphead focusRZL dzs
      fz' = fromZ $ Z ls' (fx-1,fy,f) ((lx+1,ly,l):rs)
      zz' = Z uzs' fz' dzs'
      dh = if lx < 0 then -1 else 1
  R -> case rs of
    [] -> Nothing
    (rx,ry,r):rs' -> Just (dh, Board zz') where
      uzs' = maphead focusRZR uzs
      dzs' = maphead focusRZR dzs
      fz' = fromZ $ Z ((rx-1,ry,r):ls) (fx+1,fy,f) rs'
      zz' = Z uzs' fz' dzs'
      dh = if rx > 0 then -1 else 1
  U -> case unconsRZs uzs of
    Nothing -> Nothing
    Just (uz, uzs') -> Just (dh, Board zz') where
      Z uls (ufx,ufy,uf) urs = uz
      fz' = fromZ $ Z uls (fx,fy-1,f) urs
      dz' = fromZ $ Z ls (ufx,ufy+1,uf) rs
      zz' = Z uzs' fz' (dz':dzs)
      dh = if ufy < 0 then -1 else 1
  D -> case unconsRZs dzs of
    Nothing -> Nothing
    Just (dz, dzs') -> Just (dh, Board zz') where
      Z dls (dfx,dfy,df) drs = dz
      fz' = fromZ $ Z dls (fx,fy+1,f) drs
      uz' = fromZ $ Z ls (dfx,dfy-1,df) rs
      zz' = Z (uz':uzs) fz' dzs'
      dh = if dfy > 0 then -1 else 1
  where
    Z uzs fz dzs = zz
    Z ls (fx,fy,f) rs = toZ fz

fromLL :: [[Int]] -> Board
fromLL xss = focusHole $ Board zz where
  n = length $ head xss
  correctPos i = let i' = i-1 in (i' `mod` n, i' `div` n)
  go (x,y) v = (x-x',y-y',v) where
    (x',y') = correctPos v

  xss' = mapWithPos go xss
  zz = Z.fromList $ fromZ . Z.fromList <$> xss'

  focusHole :: Board -> Board
  focusHole = goR where
    goR b = if view b == 0
      then b
      else case focus R b of
        Nothing -> goL $ fromJust (focus D b)
        Just b' -> goR b'
    goL b = if view b == 0
      then b
      else case focus L b of
        Nothing -> goR $ fromJust (focus D b)
        Just b' -> goL b'

  focus :: Dir -> Board -> Maybe Board
  focus dir (Board zz) = case dir of
    L -> case ls of
      [] -> Nothing
      (l:ls') -> Just . Board $ zz' where
        uzs' = maphead focusRZL uzs
        dzs' = maphead focusRZL dzs
        fz' = fromZ $ Z ls' l (f:rs)
        zz' = Z uzs' fz' dzs'
    R -> case rs of
      [] -> Nothing
      (r:rs') -> Just . Board $ zz' where
        uzs' = maphead focusRZR uzs
        dzs' = maphead focusRZR dzs
        fz' = fromZ $ Z (f:ls) r rs'
        zz' = Z uzs' fz' dzs'
    U -> case unconsRZs uzs of
      Nothing -> Nothing
      Just (uz, uzs') -> Just . Board $ zz' where
        zz' = Z uzs' (fromZ uz) (fz:dzs)
    D -> case unconsRZs dzs of
      Nothing -> Nothing
      Just (dz, dzs') -> Just . Board $ zz' where
        zz' = Z (fz:uzs) (fromZ dz) dzs'
    where
      Z uzs fz dzs = zz
      Z ls f rs = toZ fz

toLL :: Board -> [[Int]]
toLL (Board zz) = Z.toList $ fmap (\(_,_,v) -> v) . Z.toList . toZ <$> zz

manhattan :: Board -> Int
manhattan (Board zz) = h where
  Z uzs fz dzs = zz
  Z ls f rs = toZ fz
  dist (x,y,_) = abs x + abs y
  h = sum (sum . map dist . Z.toList . toZ <$> uzs)
    + sum (sum . map dist . Z.toList . toZ <$> dzs)
    + sum (dist <$> ls)
    + sum (dist <$> rs)

view :: Board -> Int
view = (\(_,_,x) -> x) . (\(RZ 0 (Z _ f _)) -> f) . Z.extractz . runBoard

mapWithPos :: ((Int,Int) -> a -> b) -> [[a]] -> [[b]]
mapWithPos f ass = go ass 0 where
  go [] _       = []
  go (as:ass) y = go' y as 0 : go ass (y+1)
  go' y [] _     = []
  go' y (a:as) x = f (x,y) a : go' y as (x+1)

-- toLLWithPos :: Board -> [[(Int,Int,Int)]]
-- toLLWithPos (Board zz) = Z.toList $ Z.toList . toZ <$> zz

successors :: Board -> [(Int, Board)]
successors b = catMaybes $ (flip move b) <$> [L,R,U,D]
