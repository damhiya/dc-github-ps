module Main where

import Data.ByteString        as BS
import Data.ByteString.Char8  as BSC

import Crypto.Hash.MD5

main = do
  xs <- BS.getLine
  BSC.putStrLn $ md5 xs
