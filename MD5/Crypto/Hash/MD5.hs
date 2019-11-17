{-# LANGUAGE OverloadedLists #-}

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

import Control.Monad
import Control.Monad.State -- strict or lazy

s :: V.Vector Int
s = [ 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22
    , 5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20
    , 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23
    , 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21
    ]

k :: V.Vector Word32
-- k = V.generate 64 $ \i -> floor(2^32 * (abs . sin . fromIntegral $ i+1) )
k = [ 0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee
    , 0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501
    , 0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be
    , 0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821
    , 0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa
    , 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8
    , 0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed
    , 0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a
    , 0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c
    , 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70
    , 0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05
    , 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665
    , 0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039
    , 0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1
    , 0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1
    , 0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391
    ]

a0 = 0x67452301 :: Word32
b0 = 0xefcdab89 :: Word32
c0 = 0x98badcfe :: Word32
d0 = 0x10325476 :: Word32

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
md5 xs = showWord128 $ execState (go . convert $ xs) (a0,b0,c0,d0) where
  trans :: V.Vector Word32 -> State (Word32,Word32,Word32,Word32) ()
  trans m = do
    forM_ ([0..15]::[Int]) $ \i -> do
      (a,b,c,d) <- get
      let f = d `xor` (b .&. (c `xor` d))
          g = i
      update i a b c d f g
    forM_ ([16..31]::[Int]) $ \i -> do
      (a,b,c,d) <- get
      let f = c `xor` (d .&. (b `xor` c))
          g = (5*i+1) `mod` 16
      update i a b c d f g
    forM_ ([32..47]::[Int]) $ \i -> do
      (a,b,c,d) <- get
      let f = b `xor` c `xor` d
          g = (3*i+5) `mod` 16
      update i a b c d f g
    forM_ ([48..63]::[Int]) $ \i -> do
      (a,b,c,d) <- get
      let f = c `xor` (b .|. (complement d))
          g = (7*i) `mod` 16
      update i a b c d f g
    where
      update :: Int -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Int ->
                State (Word32,Word32,Word32,Word32) ()
      update i a b c d f g = do
        let f' = f + a + (k!i) + (m!g)
            a' = d
            b' = b + rotateL f' (s!i)
            c' = b
            d' = c
        put (a',b',c',d')

  go :: [V.Vector Word32] -> State (Word32, Word32, Word32, Word32) ()
  go [] = return ()
  go (m:ms) = do
    (a,b,c,d) <- get
    let (a',b',c',d') = execState (trans m) (a,b,c,d)
    put (a+a',b+b',c+c',d+d')
    go ms

  showWord128 :: (Word32,Word32,Word32,Word32) -> BS.ByteString
  showWord128 (x,y,z,w) = BS16.encode . BSL.toStrict . BSB.toLazyByteString $ builder where
    builder = BSB.word32LE x
           <> BSB.word32LE y
           <> BSB.word32LE z
           <> BSB.word32LE w
