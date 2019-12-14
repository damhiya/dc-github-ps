module Data.JSONParser where

import Prelude hiding (exponent, toInteger)
import Data.Char
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.JSON as J
import Text.Parsec hiding (string, digit)
import Text.Parsec.Text (Parser)
import qualified Text.Parsec.Text as P
import qualified Text.Parsec.Char as P

json :: Parser J.Value
json = element

value :: Parser J.Value
value = try (J.Object <$> object)
    <|> try (J.Array  <$> array )
    <|> try (J.String <$> string)
    <|> try (J.Number <$> number)
    <|> try (P.string "true"  >> return J.True)
    <|> try (P.string "false" >> return J.False)
    <|> (P.string "null" >> return J.Null)

object :: Parser [(T.Text, J.Value)]
object = try (P.char '{' >> ws >> P.char '}' >> return [])
     <|>     (P.char '{' >> members <* P.char '}')

members :: Parser [(T.Text, J.Value)]
members = do
  p <- member
  ps <- many (P.char ',' >> member)
  return (p:ps)

member :: Parser (T.Text, J.Value)
member = (,)
     <$> (ws >> string <* ws)
     <*  P.char ':'
     <*> element

array :: Parser [J.Value]
array = try (P.char '[' >> ws >> P.char ']' >> return [])
    <|>     (P.char '[' >> elements <* P.char ']')

elements :: Parser [J.Value]
elements = do
  e <- element
  es <- many (P.char ',' >> element)
  return (e:es)

element :: Parser J.Value
element = ws >> value <* ws

string :: Parser T.Text
string = P.char '\"' >> T.pack <$> characters <* P.char '\"'

characters :: Parser String
characters = many character

character :: Parser Char
character = try (satisfy p)
        <|> (P.char '\\' >> escape)
  where
    p c = let o = ord c
          in 0x20 <= o && o <= 0x10ffff && c /= '\"' && c /= '\\'

escape :: Parser Char
escape = try (P.char '\"' >> return '\"')
     <|> try (P.char '\\' >> return '\\')
     <|> try (P.char '/'  >> return '/')
     <|> try (P.char 'b'  >> return '\b')
     <|> try (P.char 'f'  >> return '\f')
     <|> try (P.char 'n'  >> return '\n')
     <|> try (P.char 'r'  >> return '\r')
     <|> try (P.char 't'  >> return '\t')
     <|> (P.char 'u' >> hex4)

hex4 :: Parser Char
hex4 = do
  h3 <- hex
  h2 <- hex
  h1 <- hex
  h0 <- hex
  let n = ((h3*16 + h2)*16 + h1)*16 + h0
  return (chr n)

hex :: Parser Int
hex = do
  c <- P.hexDigit
  let o = ord c
  if o >= 97
    then return (o - ord 'a' + 10)
    else if o >= 65
      then return (o - ord 'A' + 10)
      else return (o - ord '0')

number :: Parser (Either Integer Double)
number = do
  i <- integer
  try (do {f <- fraction; e <- exponent; return (Right $ float i f e)}) <|> return (Left i)
  where
    float i f e = (fromInteger i + f) * 10 ** fromIntegral e

integer :: Parser Integer
integer = do
  s <- sign
  ds <- num
  let n = toInteger ds
  if s
    then return (negate n)
    else return n
  where
    sign = try (P.char '-' >> return True)
       <|> return False
    num  = try ((:) <$> onenine <*> digits)
       <|> (:[]) <$> digit
    toInteger [x] = fromIntegral x
    toInteger (x:xs) = 10 * (fromIntegral x) + toInteger xs

digits :: Parser [Int]
digits = many1 digit

digit :: Parser Int
digit = do
  c <- P.satisfy (\c -> c >= '0' && c <= '9')
  return (ord c - ord '0')

onenine :: Parser Int
onenine = do
  c <- P.satisfy (\c -> c >= '1' && c <= '9')
  return (ord c - ord '0')

fraction :: Parser Double
fraction = do
  P.char '.'
  ds <- digits
  let f = toFraction ds
  return f
  where
    toFraction [x] = fromIntegral x
    toFraction (x:xs) = fromIntegral x + (toFraction xs) / 10.0

exponent :: Parser Int
exponent = try exponent' <|> return 0
  where
    exponent' = do
      try (P.char 'E') <|> P.char 'e'
      s <- sign
      ds <- digits
      let e = toInt ds
      if s
        then return (-e)
        else return e
    sign = try (P.char '+' >> return False)
       <|> try (P.char '-' >> return True)
       <|> return False
    toInt [x] = x
    toInt (x:xs) = 10*x + toInt xs

ws :: Parser ()
ws = try (P.char ' '  >> ws)
 <|> try (P.char '\t' >> ws)
 <|> try (P.char '\n' >> ws)
 <|> try (P.char '\r' >> ws)
 <|> return ()
