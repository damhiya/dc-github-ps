{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import Control.Monad
import System.Environment

import Crypto.Hash.MD5

main :: IO ()
main = do
  fs <- getArgs
  if fs == []
    then do
      x <- BS.getLine
      putStr . show . md5 $ x
      putStr "  stdin\n"
    else forM_ fs $ \f -> do
      x <- BS.readFile f
      putStr . show . md5 $ x
      putStr "  "
      putStrLn f
