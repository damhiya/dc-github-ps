module Main where

import Data.Word
import Data.Bits ((.|.), (.&.))
import Data.Traversable
import Data.List (span)

import qualified Data.IntSet                  as S

import qualified Data.Vector                  as V (Vector, MVector)
import qualified Data.Vector.Unboxed          as U (Vector, MVector, Unbox)
import qualified Data.Vector.Generic          as V hiding (Vector)
import qualified Data.Vector.Generic.Mutable  as M hiding (MVector)

import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.STRef

import Data.List.Split
import System.Environment

type Cell = Word8
bottom  = 0 :: Cell
blank   = 1 :: Cell
fill    = 2 :: Cell
top     = 3 :: Cell

cor :: Cell -> Cell -> Cell
cor  = (.|.)

cand :: Cell -> Cell -> Cell
cand = (.&.)

type LineData = [Int]
type Line = U.Vector Cell

choose :: Int -> Int -> Int
choose n k = go n (min (n-k) k) where
  go n 0 = 1
  go 0 k = 0
  go n k = go (n-1) (k-1) `div` k' * n' where
    m = gcd n k
    n' = n `div` m
    k' = k `div` m

mchoose :: Int -> Int -> Int
mchoose n k = choose (n+k-1) k

-- msetAll :: Int -> Int -> [[Int]]
-- msetAll 0 0 = [[]]
-- msetAll 0 k = []
-- msetAll 1 k = [[k]]
-- msetAll n k = concatMap (\x -> map (x:) (msetAll (n-1) (k-x))) [k,k-1..0]

msets :: Int -> Int -> [Int] -> [[Int]]
msets _ _ [] = []
msets 0 0 [0] = [[]]
msets 1 k [0] = [[k]]
msets n k is = concat . snd $ mapAccumL go (is,0) [0..k] where
  go (is,c) y = ((is',c'), rs) where
    x  = k - y
    c' = c + mchoose (n-1) y
    (js,is') = span (<c') is
    rs = map (x:) $ msets (n-1) y (map (subtract c) js)

mapi :: (a -> a) -> [a] -> [a]
mapi f (x:xs) = x : go xs where
  go xs@[x] = xs
  go (x:xs) = f x : go xs

writeN :: U.Unbox a => U.MVector s a -> Int -> Int -> a -> ST s ()
writeN v i n x = forM_ (take n [i..]) $ \i -> M.write v i x

makeLines :: Int -> LineData -> [Int] -> [(Int, Line)]
makeLines r fs is = if null fs
  then zip is [V.replicate r blank]
  else zip is (map go ss)
  where
    blockNum = length fs
    blankNum = r - sum fs
    gapNum   = blockNum - 1
    n = blockNum + 1
    k = blankNum - gapNum
    ss = msets n k is
    
    go s = V.create $ do
      v <- M.unsafeNew r :: ST s (U.MVector s Cell)
      flip execStateT 0 $ do
        get >>= (\i -> lift $ writeN v i h blank) >> modify (+h)
        (\f -> zipWithM_ f fs bs) $ \f b -> do
          get >>= (\i -> lift $ writeN v i f fill)  >> modify (+f)
          get >>= (\i -> lift $ writeN v i b blank) >> modify (+b)
      return v
      where
        h:bs = mapi (+1) s

bisect :: [a] -> ([a], [a])
bisect xs = splitAt (length xs `div` 2) xs

solveNonogram :: V.Vector LineData -> V.Vector LineData -> [[Line]]
solveNonogram rds cds = runST $ do
  let rq = S.fromAscList [0..r-1]
      cq = S.fromAscList [0..c-1]
  rcs <- M.replicate r [0..]
  ccs <- M.replicate r [0..]
  board <- M.replicate (r*c) top
  solutions <- go rq cq rcs ccs board
  mapM format solutions
  where
    r = V.length rds
    c = V.length cds
    
    format board = mapM (readRow board) [0..r-1]

    go :: S.IntSet ->
          S.IntSet ->
          V.MVector s [Int] ->
          V.MVector s [Int] ->
          U.MVector s Cell ->
          ST s [U.MVector s Cell]
    go rq cq rcs ccs board = do
      frcs <- V.freeze rcs
      fccs <- V.freeze ccs
      if V.all done frcs -- && V.all done fccs
        then return [board]
        else case (S.minView rq, S.minView cq) of
          (Just (i,rq'), _           ) -> updateRow i rq'
          (_           , Just (j,cq')) -> updateCol j cq'
          (Nothing     , Nothing     ) -> branch
      where
        done = null . tail

        updateRow i rq' = do
          cs   <- makeLines c (rds V.! i) <$> M.read rcs i
          line <- readRow board i
          let cs' = filterCands line cs
          if null cs'
            then return []
            else do
              let line' = foldCands (map snd cs')
                  cq'   = S.union cq (S.fromList $ difference line line')
              M.write rcs i (map fst cs')
              writeRow board i line'
              go rq' cq' rcs ccs board
            
        updateCol j cq' = do
          cs   <- makeLines r (cds V.! j) <$> M.read ccs j
          line <- readCol board j
          let cs' = filterCands line cs
          if null cs'
            then return []
            else do
              let line' = foldCands (map snd cs')
                  rq'   = S.union rq (S.fromList $ difference line line')
              M.write ccs j (map fst cs')
              writeCol board j line'
              go rq' cq' rcs ccs board

        branch = do
          frcs <- V.freeze rcs
          let i = V.ifoldr f (error "brach") frcs
              (cs1,cs2) = bisect (frcs V.! i)
              rq' = S.singleton i
              cq' = S.empty
          rcs'   <- M.clone rcs
          ccs'   <- M.clone ccs
          board' <- M.clone board
          M.write rcs  i cs1
          M.write rcs' i cs2
          s1 <- go rq' cq' rcs  ccs  board
          s2 <- go rq' cq' rcs' ccs' board'
          
          return (s1 ++ s2)
          where
            f i cs j = if done cs
              then j
              else i

        filterCands :: Line -> [(Int, Line)] -> [(Int, Line)]
        filterCands base = filter p
          where
            p c = execState (V.zipWithM (\x y -> modify (&& (x `cand` y /= bottom))) base (snd c)) True

        foldCands :: [Line] -> Line
        foldCands (c:cs) = V.create $ do
          v <- V.thaw c
          forM_ cs $ \c -> V.imapM_ (\i x -> M.modify v (`cor` x) i) c
          return v
        
        difference :: Line -> Line -> [Int]
        difference u v = execState (V.izipWithM_ m u v) [] where
          m :: Int -> Cell -> Cell -> State [Int] ()
          m i x y | x == y    = return ()
                  | otherwise = modify (i:)

    readRow :: U.MVector s Cell -> Int -> ST s Line
    readRow board i = V.freeze $ M.slice (c*i) c board
    
    writeRow :: U.MVector s Cell -> Int -> Line -> ST s ()
    writeRow board i = V.imapM_ (\j x -> M.write board (c*i+j) x)

    readCol :: U.MVector s Cell -> Int -> ST s Line
    readCol board j = do
      v <- M.new r
      forM_ [0..r-1] $ \i -> do
        x <- M.read board (c*i+j)
        M.write v i x
      V.unsafeFreeze v

    writeCol :: U.MVector s Cell -> Int -> Line -> ST s ()
    writeCol board j = V.imapM_ (\i x -> M.write board (c*i+j) x)

showBoard :: [Line] -> String
showBoard board = unlines $ map (V.foldr (\x -> (showCell x ++)) "") board
  where
    showCell x
      | x == bottom = "XX"
      | x == blank  = ".."
      | x == fill   = "##"
      | x == top    = "??"

parseFile :: String -> (V.Vector LineData, V.Vector LineData)
parseFile input = (rds, cds) where
  [r,c]:ns = map read . filter (/="") . splitOn " " <$> splitOn "\n" input
  (xs, ys) = splitAt r ns
  rds = V.fromList xs
  cds = V.fromList (take c ys)

main = do
  args <- getArgs
  input <- readFile (head args)
  let (rds, cds) = parseFile input
  mapM_ (putStrLn . showBoard) (solveNonogram rds cds)
