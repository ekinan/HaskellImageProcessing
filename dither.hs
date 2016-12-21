{-#
  OPTIONS_GHC -XFlexibleContexts
#-}
module Dither 
(
  ditherThreshPixels,
  ditherConstBrightPixels,
  ditherRandomPixels,
  ditherClusterPixels,
  ditherFSGrayPixels,
  ditherFSColorPixels
) where
-- Library imports
import Graphics.UI.WX
import Graphics.UI.WXCore
import Data.Maybe 
import qualified Data.Array.IArray as IA
import qualified Data.List as L
import qualified Data.MultiSet as MS
import qualified Data.Map as MP
import Control.Monad.Random
import Control.Monad as CM
import Data.NumInstances.Tuple
import Safe

-- Custom file imports
import Utilities
import Recolor

-- Dithers a grayscale value based on some threshold 
ditherThreshGrayVal :: (Num a, Ord a) => a -> a -> a
ditherThreshGrayVal val gVal
 | gVal <= val = 0
 | otherwise   = 255

-- Do basic threshold dithering on a given pixel array
ditherThreshPixels :: (IA.IArray IA.Array Color) => SimplePixelOp
ditherThreshPixels px = IA.amap (fromGrayVal . ditherThreshGrayVal 127) $ toGrayscale px

-- Code for constant brightness dithering. What it does is it finds the average
-- intensity of all of the pixels, and then finds a threshold that's brighter than
-- (1 - I) of the pixels (to keep the average brightness constant).
ditherConstBrightPixels :: (IA.IArray IA.Array Color) => SimplePixelOp
ditherConstBrightPixels pxs = IA.amap (fromGrayVal . ditherThreshGrayVal threshVal) gPxs
 where -- g stands for gray, s stands for sorted
  gPxs = toGrayscale pxs
  sPxs = L.sort $ map toInteger $ fromMatrix $ gPxs
  sz = fromIntegral $ L.length sPxs
  avgGrayCol = (fromIntegral $ L.sum sPxs) / sz
  avgIntens = avgGrayCol / 255.0
  threshIndex = floor ((1.0 - avgIntens)*sz)
  threshVal = fromIntegral $ sPxs !! threshIndex

-- Code for random dithering. Here, the given grayscale pixel is summed by some
-- randomly generated value between (-51, 51) and then thresholded.
ditherRandomGrayVal :: Int -> IO (Int)
ditherRandomGrayVal gVal
  = do r <- getRandomR (-51, 51)
       return $ ditherThreshGrayVal 127 (gVal + r)

-- Does random dithering on an entire set of pixels. Basically does
-- a generic foldM of the method where a unique random value is generated
-- for each pixel.
ditherRandomPixels :: (IA.IArray IA.Array Color) => RandPixelOp
ditherRandomPixels pxs
  = do dVals  <- CM.foldM ditherRandomGrayVals [] $ toGrayscale pxs 
       return $ IA.listArray (IA.bounds pxs) $ map fromGrayVal $ reverse dVals
 where
  ditherRandomGrayVals :: [Int] -> Int -> IO ([Int])
  ditherRandomGrayVals vs v
    = do dV <- ditherRandomGrayVal v
         return (dV:vs)

-- Code for cluster dithering

-- Dithers a gray scale image given an arbitrary mask. For a pixel
-- at (x, y), it is dithered based on the value at I[x%C][y%R], where
-- R and C are the rows and columns of the mask.
ditherClusterGrayscale :: (IA.IArray IA.Array Int) 
                       => Matrix Double 
                       -> Grayscale
                       -> Grayscale
ditherClusterGrayscale mask gvs = IA.listArray (IA.bounds gvs)  dGvs
 where
  (r, c) = getRowCol mask

  ditherClusterPixel :: Point -> Int
  ditherClusterPixel pt = floor $ ditherThreshGrayVal (mask IA.! mI) gVal
   where
    mI = point (pointX pt `mod` c) (pointY pt `mod` r) 
    gVal = (fromIntegral $ gvs IA.! pt) :: Double

  dGvs = map (ditherClusterPixel) $ IA.indices gvs


-- Performs cluster dithering on the given set of pixels using a pre-defined
-- mask.
ditherClusterPixels :: (IA.IArray IA.Array Color) => SimplePixelOp
ditherClusterPixels pxs = IA.amap fromGrayVal $ ditherClusterGrayscale mask $ toGrayscale pxs
 where
  mask_e =map (*255) 
           [
             0.7500, 0.3750, 0.6250, 0.2500,
             0.0625, 1.0000, 0.8750, 0.4375,
             0.5000, 0.8125, 0.9375, 0.1250,
             0.1875, 0.5625, 0.3125, 0.6875
           ]
  mask = toMatrix 4 4 mask_e

-- Code for Floyd Steinberg Dithering

{-
The general Floyd-Steinberg dithering algorithm works as follows. The idea is
that at every pixel you figure out the closest palette color there, and then find
the error of that pixel from the closest color. So if we had a pixel value of
232, and our closest color was 150, the error would be 232 - 150 = 82. Then we
take this error and propagate it to our neighbor across from us, and the three
neighbors below us (for an arbitrary centered pixel).

The code here modifies the above algorithm slightly by noting that we can, at
any given pixel, determine which pixels its final, accummulated value will
depend on (the ones whose error terms get propagated). By incorporating this
notion, we can use a reasonably fast algorithm to do the dithering.
-}

-- Computes the final accummulated value after FS dithering for a single pixel.
ditherFSPixel :: (ColorRGB -> ColorRGB)
               -> (Point -> ColorRGB)
               -> (Point -> [(Point, Double)])
               -> MP.Map Point ColorRGB
               -> Point 
               -> MP.Map Point ColorRGB
ditherFSPixel closestPaletteRGB getActualColorRGB dependentPixels acc_pxsRGB pt 
  = MP.insert pt acc_pxRGB acc_pxsRGB
 where
  computeContribution (dpt, em) = (em*er, em*eg, em*eb)
   where
    acc_dpxRGB = acc_pxsRGB MP.! dpt
    (er, eg, eb) = acc_dpxRGB - (closestPaletteRGB acc_dpxRGB)


  tot_e = L.sum $ map computeContribution $ dependentPixels pt
  acc_pxRGB = getActualColorRGB pt + tot_e

-- Dithers an entire row
ditherFSRow :: (ColorRGB -> ColorRGB)
            -> (Point -> ColorRGB)
            -> MP.Map Point ColorRGB
            -> ([Point], Point -> [(Point, Double)])
            -> MP.Map Point ColorRGB
ditherFSRow closestPaletteRGB getActualColorRGB acc_pxsRGB (pts, dependentPixels)
  = L.foldl (ditherFSPixel closestPaletteRGB getActualColorRGB dependentPixels) acc_pxsRGB pts


-- Dithers the pixels given some function to select the closest color, and the
-- pixel colors as floating point values (to make the dithering easier). Note
-- that the dithering is zig-zag, meaning we go from left to right on even rows
-- (zero-indexed), and then right-to-left on odd rows. Our algorithm generates
-- a list of rows to represent this zig-zaggy nature.
--
-- Note that, per the description of the algorithm above, we have a function to
-- generate all of the dependeny pixels.
ditherFSPixels :: (IA.IArray IA.Array Color) 
               => (ColorRGB -> ColorRGB) 
               -> Matrix ColorRGB 
               -> ColorPixels
ditherFSPixels closestPaletteColorRGB pxsRGB = IA.array (IA.bounds pxsRGB) $ MP.assocs acc_pxs
 where
  getDependentPixels s (Point x y)
    = filter (isInsideMatrix pxsRGB . fst)
      [
        (point (x - s) (y - 1), 3.0/16.0),
        (point x (y - 1), 5.0/16.0),
        (point (x + s) (y - 1), 1.0/16.0),
        (point (x - s) y, 7.0/16.0)
      ] 
 
  (r, c) = getRowCol pxsRGB

  createFSRow re
    | even re   = ([Point ce re | ce <- [0..(c-1)]], getDependentPixels 1)
    | otherwise = ([Point ce re | ce <- [(c-1),(c-2)..0]], getDependentPixels (-1))

  fsRows = [createFSRow re | re <- [0..(r-1)]]

  getActualColorRGB = (pxsRGB IA.!)
  acc_pxsRGB = foldl (ditherFSRow closestPaletteColorRGB getActualColorRGB) MP.empty fsRows
  acc_pxs = MP.map (fromRGB . closestPaletteColorRGB) $ acc_pxsRGB

-- Does grayscale dithering of a color image. Note the closest palette color is
-- just the grayscale pixel value thresholded by 127 (intensity 0.5).
ditherFSGrayPixels :: (IA.IArray IA.Array Color) => SimplePixelOp
ditherFSGrayPixels pxs = ditherFSPixels closestGrayColorRGB $ IA.amap toRGB $ toGrayPixels pxs
 where
  closestGrayColorRGB (r, _, _) = (greyVal, greyVal, greyVal)
   where
    greyVal = ditherThreshGrayVal 127 r

-- Helper method to compute the closest color to a given primary value,
-- using a table of representative colors. Basically, it takes the color
-- and then finds which bin it will get mapped to. Then it figures out which
-- is the closest bin from the neighboring bins (since the representative
-- colors of each bin are not at the center), and chooses the color of that
-- bin as the final one.
closestColorPrimary :: [Double] -> Double -> Double
closestColorPrimary tbl val
  | val <= 0.0 = tbl !! 0
  | val >= 255.0 = lst
  | otherwise = clst_val
 where
  computeDiffColorPair bin = (abs (col-val), col)
   where
    col = tbl !! bin

  lvls = L.length tbl
  lst = tbl !! (lvls - 1)

  bin = floor $ val / fromIntegral (256 `div` lvls)

  pos_bins = filter (\x -> (0 <= x) &&  (x < lvls)) [bin - 1, bin, bin + 1]
  clst_val = snd $ L.minimum $ map computeDiffColorPair pos_bins

-- Does color dithering of a color image
ditherFSColorPixels :: (IA.IArray IA.Array Color) => SimplePixelOp
ditherFSColorPixels pxs = ditherFSPixels closestColorRGB $ IA.amap toRGB pxs
 where
  rg_tbl = [0, 36, 73, 109, 146, 182, 219, 255]
  b_tbl = [0, 85, 170, 255]

  closestColorPrimaryRG = closestColorPrimary rg_tbl
 
  closestColorRGB (r, g, b) 
    = (closestColorPrimaryRG r, closestColorPrimaryRG g, closestColorPrimary b_tbl b)
