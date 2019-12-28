{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status

import Data.Aeson

newtype StarCount = StarCount {toInt :: Int} deriving Show

instance FromJSON StarCount where
  parseJSON = withObject "StarCount" $ \v ->
    StarCount <$> v .: "stargazers_count"

main = do
  name <- getLine

  manager <- newManager tlsManagerSettings
  _request <- parseRequest $ "https://api.github.com/users/" ++ name ++ "/repos"
  let headers = [("User-Agent", "Main")]
      request = _request {requestHeaders = headers}
  response <- httpLbs request manager

  if responseStatus response == status200
    then case decode $ responseBody response of
      Nothing -> putStrLn "parse fail"
      Just xs -> print (sum . map toInt $ xs)
    else putStrLn "network error"
