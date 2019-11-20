{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE UnboxedTuples    #-}
{-# LANGUAGE TemplateHaskell  #-}

module Crypto.Hash.MD5 (MD5Digest(..), md5) where

import GHC.Base
import GHC.Word
import GHC.ForeignPtr

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe   as BS
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.ByteString.Builder  as BSB

import Crypto.Hash.Template

data MD5Digest  = MD5Digest {-# UNPACK #-} !Word32
                            {-# UNPACK #-} !Word32
                            {-# UNPACK #-} !Word32
                            {-# UNPACK #-} !Word32

type Pack#      = (# Word#, Word#, Word#, Word# #)
type HState# s  = (# State# s, Pack# #)
type Operation# = Word# -> Word# -> Word# -> Word#

instance Show MD5Digest where
  show (MD5Digest (W32# x) (W32# y) (W32# z) (W32# w)) = r where
    r =
      [ c0 x, c1 x, c2 x, c3 x, c4 x, c5 x, c6 x, c7 x
      , c0 y, c1 y, c2 y, c3 y, c4 y, c5 y, c6 y, c7 y
      , c0 z, c1 z, c2 z, c3 z, c4 z, c5 z, c6 z, c7 z
      , c0 w, c1 w, c2 w, c3 w, c4 w, c5 w, c6 w, c7 w
      ]
    c0 x = toChar (uncheckedShiftRL# x 4#  `and#` 15##)
    c1 x = toChar (uncheckedShiftRL# x 0#  `and#` 15##)
    c2 x = toChar (uncheckedShiftRL# x 12# `and#` 15##)
    c3 x = toChar (uncheckedShiftRL# x 8#  `and#` 15##)
    c4 x = toChar (uncheckedShiftRL# x 20# `and#` 15##)
    c5 x = toChar (uncheckedShiftRL# x 16# `and#` 15##)
    c6 x = toChar (uncheckedShiftRL# x 28# `and#` 15##)
    c7 x = toChar (uncheckedShiftRL# x 24# `and#` 15##)
    toChar x = case x of
      0##  -> '0'
      1##  -> '1'
      2##  -> '2'
      3##  -> '3'
      4##  -> '4'
      5##  -> '5'
      6##  -> '6'
      7##  -> '7'
      8##  -> '8'
      9##  -> '9'
      10## -> 'a'
      11## -> 'b'
      12## -> 'c'
      13## -> 'd'
      14## -> 'e'
      15## -> 'f'

cast :: Pack# -> MD5Digest
cast (# x, y, z, w #) = MD5Digest (W32# x) (W32# y) (W32# z) (W32# w)

addp# :: Pack# -> Pack# -> Pack#
addp# x y = (# z1, z2, z3, z4 #) where
  (# x1, x2, x3, x4 #) = x
  (# y1, y2, y3, y4 #) = y
  z1 = x1 `plusWord#` y1
  z2 = x2 `plusWord#` y2
  z3 = x3 `plusWord#` y3
  z4 = x4 `plusWord#` y4

foldl# :: (HState# s -> a -> HState# s) -> HState# s -> [a] -> HState# s
foldl# f# y [] = y
foldl# f# y (x:xs) = foldl# f# (f# y x) xs

(>>>#) :: (HState# s -> HState# s) -> (HState# s -> HState# s) -> (HState# s -> HState# s)
f# >>># g# = \x -> g# (f# x)

convert :: BS.ByteString -> (ForeignPtr Word32, Int, ForeignPtr Word32, Int)
convert xs = (p0,n0,p1,n1) where
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

md5 :: BS.ByteString -> MD5Digest
md5 xs = cast result where
  a = 0x67452301##
  b = 0xefcdab89##
  c = 0x98badcfe##
  d = 0x10325476##

  (ForeignPtr p0 c0, I# n0, ForeignPtr p1 c1, I# n1) = convert xs

  calc s = go# p1 n1 ( go# p0 n0 (# s, (# a, b, c, d #) #) )
  result =  case runRW# calc of
              (# s1, x #) -> case touch# c0 s1 of
                s2 -> case touch# c1 s2 of
                  s3 -> x
  
  go# :: Addr# -> Int# -> HState# s -> HState# s
  go# p 0# hs = hs
  go# p n hs = go# (plusAddr# p 64#) (n -# 1#) (# s', x `addp#` x' #) where
    (# _, x  #) = hs
    (# s',x' #) = trans# p hs

  trans# :: Addr# -> HState# s -> HState# s
  trans# p = ($(tfold xs1) (update# f1#))
        >>># ($(tfold xs2) (update# f2#))
        >>># ($(tfold xs3) (update# f3#))
        >>># ($(tfold xs4) (update# f4#))
    where
      f1#,f2#,f3#,f4# :: Operation#
      f1# b c d = d `xor#` (b `and#` (c `xor#` d))
      f2# b c d = c `xor#` (d `and#` (b `xor#` c))
      f3# b c d = b `xor#` c `xor#` d
      f4# b c d = c `xor#` (b `or#` not# d)

      update# :: Operation# -> (Int,Int,Word32) -> HState# s -> HState# s
      update# f# (g, r, k) (# s, (# a, b, c, d #) #) = (# s', (# a', b', c', d' #) #) where
        I#   g# = g
        I#   r# = r
        W32# k# = k
        (# s', m #) = readWord32OffAddr# p g# s
        
        f' = narrow32Word# (f# b c d `plusWord#` a `plusWord#` k# `plusWord#` m)
        a' = d
        b' = narrow32Word# (b `plusWord#` rotL# f' r#)
        c' = b
        d' = c

      not# :: Word# -> Word#
      not# x = x `xor#` m where W32# m = maxBound

      rotL# :: Word# -> Int# -> Word#
      rotL# x i = narrow32Word# (hi `or#` lo) where
        hi = x `uncheckedShiftL#` i
        lo = x `uncheckedShiftRL#` (32# -# i)
