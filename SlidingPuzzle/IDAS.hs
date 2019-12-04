{-# LANGUAGE ScopedTypeVariables #-}

module IDAS where

import Debug.Trace

-- not for general purpose
-- n -> [(delta heuristic, successor)]
-- initial heuristic
-- initial node
runIDAS :: forall a. (a -> [(Int,a)]) -> Int -> a -> [a]
runIDAS succs h0 n0 = loop h0 where
  loop b = case go [n0] 0 h0 b of
    Left b' -> loop b'
    Right p -> reverse p
  -- path g h bound
  go :: [a] -> Int -> Int -> Int -> Either Int [a]
  go p g h b = --traceShow h $
    if h == 0
      then Right p
      else if f > b
        then Left f
        else fold1 rs
    where
      f = g + h
      rs = map (\(dh, n') -> go (n':p) (g+1) (h+dh) b) (succs $ head p)
      fold1 [r] = r
      fold1 (r:rs) = case r of
        Left b1 -> case fold1 rs of
          Left b2 -> Left $ min b1 b2
          Right p -> Right p
        Right p -> Right p
