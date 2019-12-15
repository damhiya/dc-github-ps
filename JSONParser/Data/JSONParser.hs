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
members = (:) <$> member <*> many (P.char ',' >> member)

member :: Parser (T.Text, J.Value)
member = (,)
     <$> (ws >> string <* ws)
     <*  P.char ':'
     <*> element

array :: Parser [J.Value]
array = try (P.char '[' >> ws >> P.char ']' >> return [])
    <|>     (P.char '[' >> elements <* P.char ']')

elements :: Parser [J.Value]
elements = (:) <$> element <*> many (P.char ',' >> element)

element :: Parser J.Value
element = ws >> value <* ws

string :: Parser T.Text
string = P.char '\"' >> T.pack <$> characters <* P.char '\"'

characters :: Parser String
characters = many character

character :: Parser Char
character = try (satisfy p) <|> (P.char '\\' >> escape)
  where
    p c = 0x20 <= o && o <= 0x10ffff && c /= '\"' && c /= '\\' where
      o = ord c

escape :: Parser Char
escape = try (P.char '\"' >> return '\"')
     <|> try (P.char '\\' >> return '\\')
     <|> try (P.char '/'  >> return '/' )
     <|> try (P.char 'b'  >> return '\b')
     <|> try (P.char 'f'  >> return '\f')
     <|> try (P.char 'n'  >> return '\n')
     <|> try (P.char 'r'  >> return '\r')
     <|> try (P.char 't'  >> return '\t')
     <|> (P.char 'u' >> hex4)

hex4 :: Parser Char
hex4 = chr <$> (cons <$> hex <*> hex <*> hex <*> hex)
  where
    cons x y z w = ((x*16 + y)*16 + z)*16 + w

hex :: Parser Int
hex = try (fromNumber <$> P.satisfy number)
  <|> try (fromUpper  <$> P.satisfy upper )
  <|> try (fromLower  <$> P.satisfy lower )
  where
    number c = '0' <= c && c <= '9'
    upper  c = 'A' <= c && c <= 'F'
    lower  c = 'a' <= c && c <= 'f'
    fromNumber c = ord c - ord '0'
    fromUpper  c = ord c - ord 'A' + 10
    fromLower  c = ord c - ord 'a' + 10

number :: Parser (Either Integer Double)
number = do
  s <- minus
  i <- integer
  makeNumber s i
  where
    makeNumber s i = try (fmap Right $ makeFloat s i <$> fraction <*> (try exponent <|> return 0))
                 <|> try (fmap Right $ makeFloat s i 0.0 <$> exponent)
                 <|> return (Left $ makeInteger s i)
    makeFloat s i f e = if s then -x else x where
      x = (fromIntegral i + f) * 10 ** fromIntegral e
    makeInteger s i = if s then -i else i

minus :: Parser Bool
minus = try (P.char '-' >> return True) <|> return False

integer :: Parser Integer
integer = toInteger 0 <$> (try ((:) <$> onenine <*> digits) <|> (:[]) <$> digit)
  where
    toInteger n [] = n
    toInteger n (x:xs) = toInteger (10*n + fromIntegral x) xs

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
fraction = P.char '.' >> toFraction <$> digits
  where
    toFraction [] = 0.0
    toFraction (x:xs) = fromIntegral x / 10.0 + (toFraction xs) / 10.0

exponent :: Parser Int
exponent = (try (P.char 'E') <|> P.char 'e') >> makeInt <$> sign <*> digits
  where
    makeInt s ds = let e = toInt 0 ds in if s then -e else e
    toInt n [] = n
    toInt n (x:xs) = toInt (10*n + x) xs

sign :: Parser Bool
sign = try (P.char '+' >> return False)
   <|> try (P.char '-' >> return True)
   <|> return False

ws :: Parser ()
ws = try (P.char ' '  >> ws)
 <|> try (P.char '\t' >> ws)
 <|> try (P.char '\n' >> ws)
 <|> try (P.char '\r' >> ws)
 <|> return ()
