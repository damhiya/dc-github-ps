module Main where

import Board
import IDAS
import Control.Monad

main = do
  let n = 4
  xss <- replicateM n readLn
  let b0 = fromLL xss
      h0 = manhattan b0
      sol = runIDAS successors h0 b0

  putChar '\n'
  mapM_ (print >=> const (putChar '\n')) sol
