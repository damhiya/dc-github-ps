module Main where

import Codec.Picture(Image, PixelRGBA8(..), writePng)
import Graphics.Rasterific
import Graphics.Rasterific.Texture

data MkTree =
  MkTree
  { origin :: Point
  , t0 :: Float
  , l0 :: Float
  , w0 :: Float
  , ta :: Float
  , tb :: Float
  , la :: Float
  , lb :: Float
  , wa :: Float
  , wb :: Float
  } 

black = PixelRGBA8 0x00 0x00 0x00 0xff
white = PixelRGBA8 0xff 0xff 0xff 0xff

renderTree :: MkTree -> Int -> Drawing PixelRGBA8 ()
renderTree mt = go origin t0 l0 w0 where
  MkTree origin t0 l0 w0 ta tb la lb wa wb = mt
  go p t l w 0 = return ()
  go p t l w d = do
    stroke w JoinRound (CapStraight 1.0, CapStraight 1.0) (line p p')
    go p' (t+ta) (l*la) (w*wa) (d-1)
    go p' (t+tb) (l*lb) (w*wb) (d-1)
    where
      p' = p + V2 (l * cos t) (l * sin t)

main :: IO ()
main = writePng "output.png" img where
  w = 1600
  h = 1200
  origin = (V2 (fromIntegral w / 2.0) (fromIntegral h))
  t0 = -pi/2
  l0 = 150.0
  w0 = 6.0
  ta = 0.3
  tb = -0.3
  la = 0.9
  lb = 0.9
  wa = 0.9
  wb = 0.9
  mt = MkTree origin t0 l0 w0 ta tb la lb wa wb
  render = withTexture (uniformTexture black) (renderTree mt 11)
  img = renderDrawing w h white render
