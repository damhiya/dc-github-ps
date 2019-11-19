{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Crypto.Hash.MD5 (md5) where

import Data.Word
import Data.Bits

import qualified Data.Vector.Storable as V

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe   as BS
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

convert :: BS.ByteString -> ([V.Vector Word32], [V.Vector Word32])
convert xs = (toVectos p0 n0, toVectos p1 n1) where
  (p0,n0,p1,n1) = trim xs

  trim :: BS.ByteString -> (ForeignPtr Word32, Int, ForeignPtr Word32, Int)
  trim xs = (p0,n0,p1,n1) where
    (ptr,offset,length) = BS.toForeignPtr xs
    p0 = plusForeignPtr ptr offset
    n0 = length `div` 64

    nzero = 64 - (length + 1 + 8) `mod` 64
    builder = BSB.byteString (BS.unsafeDrop (n0 * 64) xs)
           <> BSB.word8       0x80
           <> BSB.byteString (BS.replicate nzero 0)
           <> BSB.word64LE   (fromIntegral $ length * 8)
    xs' = BSL.toStrict . BSB.toLazyByteString $ builder

    (ptr',offset',length') = BS.toForeignPtr xs'
    p1 = plusForeignPtr ptr' offset'
    n1 = length' `div` 64
  
  toVectos :: ForeignPtr Word32 -> Int -> [V.Vector Word32]
  toVectos p n = map (\i -> V.unsafeFromForeignPtr p (i*16) 16) [0..n-1]

md5 :: BS.ByteString -> BS.ByteString
md5 xs = showWord128 . go ms2 . go ms1 $ (a0,b0,c0,d0) where
  (ms1, ms2) = convert xs

  go :: [V.Vector Word32] -> Pack -> Pack
  go ms p = foldl (\p m -> p `addp` trans m p) p ms

  trans :: V.Vector Word32 -> Pack -> Pack
  trans m = ($(tfold xs1) $ update f1)
        >>> ($(tfold xs2) $ update f2)
        >>> ($(tfold xs3) $ update f3)
        >>> ($(tfold xs4) $ update f4)
    where
      f1 b c d = d `xor` (b .&. (c `xor` d))
      f2 b c d = c `xor` (d .&. (b `xor` c))
      f3 b c d = b `xor` c `xor` d
      f4 b c d = c `xor` (b .|. (complement d))

      update f (a,b,c,d) (g,s,k) = (a',b',c',d') where
        f' = f b c d + a + k + V.unsafeIndex m g
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
