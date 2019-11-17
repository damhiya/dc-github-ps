{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BSC

import Control.Monad

import System.Environment

import Crypto.Hash.MD5

main :: IO ()
main = do
  fs <- getArgs
  forM_ fs $ \f -> do
    x <- BS.readFile f
    BSC.putStr (md5 x)
    BSC.putStr "  "
    putStrLn f
