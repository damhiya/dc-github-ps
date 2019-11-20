{-# LANGUAGE TemplateHaskell #-}

module Crypto.Hash.Template where

import Data.Word
import Language.Haskell.TH.Syntax

g1,g2,g3,g4 :: [Int]
g1 = map (\i -> i               ) [0 ..15]
g2 = map (\i -> (5*i+1) `mod` 16) [16..31]
g3 = map (\i -> (3*i+5) `mod` 16) [32..47]
g4 = map (\i -> (7*i)   `mod` 16) [48..63]

s1,s2,s3,s4 :: [Int]
s1 = [7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22]
s2 = [5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20]
s3 = [4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23]
s4 = [6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21]

k1,k2,k3,k4 :: [Word32]
k1 =  [ 0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee
      , 0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501
      , 0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be
      , 0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821
      ]
k2 =  [ 0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa
      , 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8
      , 0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed
      , 0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a
      ]
k3 =  [ 0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c
      , 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70
      , 0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05
      , 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665
      ]
k4 =  [ 0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039
      , 0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1
      , 0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1
      , 0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391
      ]

xs1,xs2,xs3,xs4 :: [(Int,Int,Word32)]
xs1 = zip3 g1 s1 k1
xs2 = zip3 g2 s2 k2
xs3 = zip3 g3 s3 k3
xs4 = zip3 g4 s4 k4

tfold :: Lift a => [a] -> Q Exp
tfold []      = [| \f y -> y |]
tfold (x:xs)  = [| \f y -> $(tfold xs) f (f x y) |]
