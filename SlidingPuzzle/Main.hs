module Main where

import Board
import IDAS

xss :: [[Int]]
xss =
  [ [5,1,3,4]
  , [2,0,6,7]
  , [9,10,12,8]
  , [13,14,11,15]
  ]

b = fromLL xss
h0 = manhattan b

main = return ()
