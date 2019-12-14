module Data.JSON where

import Prelude hiding (True, False, String)
import qualified Prelude as P
import Data.Map
import Data.Text

data Value = Object [(Text, Value)]
           | Array [Value]
           | String Text
           | Number (Either Integer Double)
           | True
           | False
           | Null

instance Show Value where
  show (Object x) = showMap x
  show (Array  x) = show x
  show (String x) = show x
  show (Number (Left  x)) = show x
  show (Number (Right x)) = show x
  show True  = "true"
  show False = "false"
  show Null  = "nul" 

showMap :: (Show a, Show b) => [(a,b)] -> P.String
showMap [] = "{}"
showMap ((x,y):xs) = ("{"++) . shows x . (":"++) . shows y . go xs $ "}" where
  go [] = id
  go ((x,y):xs) = (", "++) . shows x . (":"++) . shows y . go xs
