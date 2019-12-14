module Main where

import System.Environment

import qualified Data.Text.IO as IO
import Data.JSON
import Data.JSONParser (json)
import Text.Parsec (parse)

main = do
  args <- getArgs
  mapM_ go args 
  where
    go f = do
      s <- IO.readFile f
      case parse json f s of
        Left  err -> print err
        Right res -> print res >> putChar '\n'
