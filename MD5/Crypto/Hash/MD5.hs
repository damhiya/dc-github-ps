{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE UnboxedTuples    #-}
{-# LANGUAGE TemplateHaskell  #-}

module Crypto.Hash.MD5 (md5) where

import GHC.Prim
import GHC.Word
import GHC.Types

import qualified Data.Vector.Storable as V

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe   as BS
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.ByteString.Builder  as BSB

import Data.ByteString.Base16 as BS16

import Foreign.ForeignPtr

import Crypto.Hash.Template

type Pack# = (# Word#,Word#,Word#,Word# #)

addp# :: Pack# -> Pack# -> Pack#
addp# x# y# = (# z1#,z2#,z3#,z4# #) where
  (# x1#,x2#,x3#,x4# #) = x#
  (# y1#,y2#,y3#,y4# #) = y#
  z1# = x1# `plusWord#` y1#
  z2# = x2# `plusWord#` y2#
  z3# = x3# `plusWord#` y3#
  z4# = x4# `plusWord#` y4#

foldl# :: (Pack# -> a -> Pack#) -> Pack# -> [a] -> Pack#
foldl# f# y# [] = y#
foldl# f# y# (x:xs) = foldl# f# (f# y# x) xs

(>>>#) :: (Pack# -> Pack#) -> (Pack# -> Pack#) -> (Pack# -> Pack#)
f# >>># g# = \x# -> g# (f# x#)

convert :: BS.ByteString -> ([V.Vector Word32], [V.Vector Word32])
convert xs = (toVectors p0 n0, toVectors p1 n1) where
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
  
  toVectors :: ForeignPtr Word32 -> Int -> [V.Vector Word32]
  toVectors p n = map (\i -> V.unsafeFromForeignPtr p (i*16) 16) [0..n-1]

md5 :: BS.ByteString -> BS.ByteString
md5 xs = showWord128 (go# ms2 (go# ms1 (# a#,b#,c#,d# #))) where
  W32# a# = 0x67452301
  W32# b# = 0xefcdab89
  W32# c# = 0x98badcfe
  W32# d# = 0x10325476

  (ms1, ms2) = convert xs

  go# :: [V.Vector Word32] -> Pack# -> Pack#
  go# ms p# = foldl# (\p# m -> p# `addp#` trans# m p#) p# ms

  trans# :: V.Vector Word32 -> Pack# -> Pack#
  trans# m = ($(tfold xs1) (update f1#))
        >>># ($(tfold xs2) (update f2#))
        >>># ($(tfold xs3) (update f3#))
        >>># ($(tfold xs4) (update f4#))
    where
      f1# b# c# d# = d# `xor#` (b# `and#` (c# `xor#` d#))
      f2# b# c# d# = c# `xor#` (d# `and#` (b# `xor#` c#))
      f3# b# c# d# = b# `xor#` c# `xor#` d#
      f4# b# c# d# = c# `xor#` (b# `or#` (d# `xor#` mb#))
      W32# mb# = maxBound

      update :: (Word# -> Word# -> Word# -> Word#) -> Pack# -> (Int,Int,Word32) -> Pack#
      update f# (# a#,b#,c#,d# #) (g,s,k) = (# a'#,b'#,c'#,d'# #) where
        W32# k# = k
        W32# m# = V.unsafeIndex m g
        I# s# = s
        f'# = narrow32Word# (f# b# c# d# `plusWord#` a# `plusWord#` k# `plusWord#` m#)
        a'# = d#
        b'# = narrow32Word# (b# `plusWord#` rotL# f'# s#)
        c'# = b#
        d'# = c#
        rotL# :: Word# -> Int# -> Word#
        rotL# x# i# = narrow32Word# (hi# `or#` lo#) where
          hi# = x# `uncheckedShiftL#` i#
          lo# = x# `uncheckedShiftRL#` (32# -# i#)

  showWord128 :: Pack# -> BS.ByteString
  showWord128 (# x#,y#,z#,w# #) = run builder where
    builder = BSB.word32LE (W32# x#)
           <> BSB.word32LE (W32# y#)
           <> BSB.word32LE (W32# z#)
           <> BSB.word32LE (W32# w#)
    run = BS16.encode . BSL.toStrict . BSB.toLazyByteString
