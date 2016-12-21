{-#
  OPTIONS_GHC -XFlexibleContexts
#-}
module Recolor 
(
  toGrayPixels,
  uniformQuantPixels,
  populosityQuantPixels
) where
-- Library imports
import Graphics.UI.WX
import Graphics.UI.WXCore
import Data.Maybe 
import qualified Data.Array.IArray as IA
import qualified Data.List as L
import qualified Data.MultiSet as MS

-- Custom file imports
import Utilities

-- Takes an array of pixels and converts them to grayscale values
toGrayPixels :: (IA.IArray IA.Array Color) => SimplePixelOp
toGrayPixels pxs = IA.amap fromGrayVal $ toGrayscale pxs

-- Returns the representative color of the bin given the level
-- of the primary
getBinRepPrimary :: Int -> Int -> Int
getBinRepPrimary lvl bin = (step*bin + step*(bin+1)) `div` 2
 where
  step = 256 `div` lvl

-- Takes a level and a primary color value, and uniformly quantizes it
uniformQuantPrimary :: Int -> Int -> Int
uniformQuantPrimary lvl val = getBinRepPrimary lvl bin
 where
  step = 256 `div` lvl
  bin  = val `div` step

-- Takes a color and returns a uniformly quantized representation of it.
uniformQuantColor :: (Int, Int, Int) -> Color -> Color
uniformQuantColor (rLvl, gLvl, bLvl) clr
  = rgb (uniformQuantPrimary rLvl $ colorRed clr) 
        (uniformQuantPrimary gLvl $ colorGreen clr) 
        (uniformQuantPrimary bLvl $ colorBlue clr)

uniformQuantPixelsLevel :: (IA.IArray IA.Array Color)
                        => (Int, Int, Int)
                        -> ColorPixels
                        -> ColorPixels
uniformQuantPixelsLevel lvls pxs = IA.amap (uniformQuantColor lvls) pxs

-- Takes an array of pixels and converts them to their uniformly quantized
-- representations. Red and green are 8 levels, blue is 4 levels.
uniformQuantPixels :: (IA.IArray IA.Array Color) => SimplePixelOp
uniformQuantPixels pxs = uniformQuantPixelsLevel (8, 8, 4) pxs

-- Code for populosity

-- Constructs the spectrum of colors for that primary given its level.
constructPrimarySpect :: Int -> [Int]
constructPrimarySpect lvl = L.map (getBinRepPrimary lvl) [0..(lvl-1)]

-- Given the primary levels, constructs the color wheel
constructColorWheel :: (Int, Int, Int) -> [Color]
constructColorWheel (rLvl, gLvl, bLvl) = [(rgb r g b) | r <- rSpect, g <- gSpect, b <- bSpect]
 where
  rSpect = constructPrimarySpect rLvl
  gSpect = constructPrimarySpect gLvl
  bSpect = constructPrimarySpect bLvl

findClosestColor :: [Color] -> Color -> Color
findClosestColor clrs cl = L.minimumBy closestColor clrs
 where
  closestColor cl1 cl2 = compare d1 d2
   where
    d1 = euclidianDistanceSq cl cl1
    d2 = euclidianDistanceSq cl cl2

{-
Does populosity quantization on the colors. The idea of the algorithm is
as values.
  1. Uniformly quantize the image down to some set of levels (to reduce
  the color space).
  
  2. Using the color space in #1, create a histogram of every color in the
  image with the color space.
  
  3. Find the top 256 most popular colors. Now for each pixel, find the closest
  color from these 256 colors using the Euclidian distance as the metric, and
  set the pixel value to that.
-}
populosityQuantPixels :: (IA.IArray IA.Array Color) => SimplePixelOp
populosityQuantPixels pxs = IA.amap (findClosestColor topColors) pxs
 where -- sq stands for sorted, quantized
  compColor cl1 cl2 = compare (toRGB cl1) (toRGB cl2)

  sqPxs 
    = MS.fromList $ L.sortBy compColor $ fromMatrix $ uniformQuantPixelsLevel (32, 32, 32) pxs 
  colorWheel = constructColorWheel (32, 32, 32)

  -- Construct the color histogram and take the top 256 colors
  topColorEntries 
    = take 256 $ L.sortBy (flip compare) $ L.map (\clr -> (MS.occur clr sqPxs, clr)) colorWheel
  
  topColors = map snd topColorEntries
