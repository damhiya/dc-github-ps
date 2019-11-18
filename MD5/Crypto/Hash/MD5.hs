{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Crypto.Hash.MD5 (md5) where

import Data.Word
import Data.Bits

import Data.Vector.Storable ((!))
import qualified Data.Vector.Storable as V

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.ByteString.Builder  as BSB

import Data.ByteString.Base16 as BS16

import Foreign.ForeignPtr

import Control.Category ((>>>))

import Crypto.Hash.Template

a0 = 0x67452301 :: Word32
b0 = 0xefcdab89 :: Word32
c0 = 0x98badcfe :: Word32
d0 = 0x10325476 :: Word32

type Pack = (Word32,Word32,Word32,Word32)

addp :: Pack -> Pack -> Pack
addp x y = z1 `seq` z2 `seq` z3 `seq` z4 `seq` (z1,z2,z3,z4) where
  (x1,x2,x3,x4) = x
  (y1,y2,y3,y4) = y
  z1 = x1 + y1
  z2 = x2 + y2
  z3 = x3 + y3
  z4 = x4 + y4

convert :: BS.ByteString -> [V.Vector Word32]
convert xs = go 64 id . padding $ xs where
  padding :: BS.ByteString -> [BS.ByteString]
  padding xs = run builder where
    n     = fromIntegral $ BS.length xs
    nzero = fromIntegral $ 64 - (n + 1 + 8) `mod` 64
    builder = BSB.byteString xs
           <> BSB.word8 0x80
           <> BSB.byteString (BS.replicate nzero 0)
           <> BSB.word64LE (n*8)
    run = BSL.toChunks . BSB.toLazyByteString

  go :: Int -> ([BS.ByteString] -> [BS.ByteString]) -> [BS.ByteString] -> [V.Vector Word32]
  go n f [] | n == 64   = []
            | otherwise = error "length mismatch"
  go n f (xs:xss) = if n > m
    then go (n-m) (f . (xs:)) xss
    else (cast . mconcat . f $ [ys]) : go 64 id (zs:xss) where
      m = BS.length xs
      (ys,zs) = BS.splitAt n xs

  cast :: BS.ByteString -> V.Vector Word32
  cast xs = V.unsafeFromForeignPtr0 ptr' 16 where
    (ptr,offset,length) = BS.toForeignPtr xs
    ptr' = plusForeignPtr ptr offset

md5 :: BS.ByteString -> BS.ByteString
md5 xs = showWord128 . go . convert $ xs where
  go :: [V.Vector Word32] -> Pack
  go ms = foldl (\p m -> p `addp` trans m p) (a0,b0,c0,d0) ms

  trans :: V.Vector Word32 -> Pack -> Pack
  trans m = ($(tfold xs1) $ \(a,b,c,d) (i,s,k) ->
              let f = d `xor` (b .&. (c `xor` d))
                  g = i
              in f `seq` g `seq` update a b c d f g s k)
        >>> ($(tfold xs2) $ \(a,b,c,d) (i,s,k) ->
              let f = c `xor` (d .&. (b `xor` c))
                  g = (5*i+1) `mod` 16
              in f `seq` g `seq` update a b c d f g s k)
        >>> ($(tfold xs3) $ \(a,b,c,d) (i,s,k) ->
              let f = b `xor` c `xor` d
                  g = (3*i+5) `mod` 16
              in f `seq` g `seq` update a b c d f g s k)
        >>> ($(tfold xs4) $ \(a,b,c,d) (i,s,k) ->
              let f = c `xor` (b .|. (complement d))
                  g = (7*i) `mod` 16
              in f `seq` g `seq` update a b c d f g s k)
    where
      update a b c d f g s k = a' `seq` b' `seq` c' `seq` d' `seq` (a',b',c',d') where
        f' = f + a + k + (m!g)
        a' = d
        b' = b + rotateL f' s
        c' = b
        d' = c

  showWord128 :: Pack -> BS.ByteString
  showWord128 (x,y,z,w) = run builder where
    builder = BSB.word32LE x
           <> BSB.word32LE y
           <> BSB.word32LE z
           <> BSB.word32LE w
    run = BS16.encode . BSL.toStrict . BSB.toLazyByteString
