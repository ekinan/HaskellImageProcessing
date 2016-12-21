{-#
  OPTIONS_GHC -XFlexibleContexts
#-}
module Filter 
(
  boxFilterPixels,
  bartlettFilterPixels,
  arbitraryGaussianFilterPixels,
  gaussianFilterPixels,
  edgeDetectPixels,
  edgeEnhancePixels
) where
-- Library imports
import Graphics.UI.WX
import Graphics.UI.WXCore
import Data.Maybe 
import qualified Data.Array.IArray as IA
import qualified Data.List as L
import Data.NumInstances.Tuple

-- Custom file imports
import Utilities

{-
This is the source file that contains all of the image filtering operations.
Many of the functions here just use the "applyFilterPixels" method to do their
calculations.
-}

{-
This function takes an RGB pixel, and clamps each color down to the range
[0, 255].
-}
clampPixel :: ColorRGB -> ColorRGB
clampPixel (r, g, b) = (clampVal r, clampVal g, clampVal b)
 where
  clampVal v
    | v <= 0.0   = 0.0
    | v >= 255.0 = 255.0
    | otherwise   = v 

{-
Convolves a pixel with a given filter. Note all this is doing is taking a
weighted average of the pixel with its neighbors, using the filter as the
weights, and then applying this average as the final value of that pixel.
-}
convolvePixel :: ColorPixels
              -> Matrix Double
              -> Point
              -> Color
convolvePixel pxs filt pt@(Point x y)
  = fromRGB $ clampPixel $ L.sum $ map computePixelContribution neighbors
 where
  (r, c) = getRowCol filt
  mid_r = r `div` 2
  mid_c = c `div` 2

  col_lims = [(-mid_c),(-mid_c+1)..mid_c]
  row_lims = [(-mid_r),(-mid_r+1)..mid_r]

  neighbors 
    = filter (isInsideMatrix pxs) [point (x - ce) (y - re) | ce <- col_lims, re <- row_lims] 

  computePixelContribution np@(Point nx ny)
    = (fVal*r, fVal*g, fVal*b)
   where
    fpt = Point (nx - x + mid_c) (ny - y + mid_r)
    fVal = filt IA.! fpt 
    (r, g, b) = (toRGB . (pxs IA.!)) np

-- Applies a filter to a bunch of pixels.
applyFilterPixels :: (IA.IArray IA.Array Color)
                  => Matrix Double
                  -> SimplePixelOp
applyFilterPixels filt pxs 
  = IA.listArray (IA.bounds pxs) $ map (convolvePixel pxs filt) (IA.indices pxs)

{-
Creates a 2-D filter from a 1-D one by taking the outer product of the
1D filter with itself.
-}
create2DFilterFrom1D :: (Fractional a, Enum a) => [a] -> Matrix a
create2DFilterFrom1D filt 
  = toMatrix sz sz $ map (/scale) [x*y | x <- filt, y <- filt]
 where
  sz = length filt
  scale = (sum filt)^2

-- Applies a 5 x 5box filter
boxFilterPixels :: (IA.IArray IA.Array Color) => SimplePixelOp
boxFilterPixels pxs = applyFilterPixels (create2DFilterFrom1D $ replicate 5 1.0) pxs

-- Applies a 4 x 4 Bartlett filter
bartlettFilterPixels :: (IA.IArray IA.Array Color) => SimplePixelOp
bartlettFilterPixels pxs = applyFilterPixels (create2DFilterFrom1D bartlett1D) pxs
 where
  bartlett1D = [1, 2, 3, 2, 1]

-- Creates an N x N Gaussian filter by first using the binomial coefficients
-- as the values of the 1-D filter, then the 2D filter creation function above
-- to create the 2-D filter.
createGaussianFilter :: (Fractional a, Enum a) => Int -> Matrix a
createGaussianFilter = create2DFilterFrom1D . createGaussianFilter1D
 where
  binomCoeff n k = foldl (\res i -> res * (n + 1 - i) / i) 1 [i | i <- [1..k]]

  createGaussianFilter1D n = [binomCoeff (nf - 1) i | i <- [0..(nf - 1)]]
   where
    nf = fromIntegral n

-- Applies an N x N Gaussian filter to the given set of pixels.
arbitraryGaussianFilterPixels :: (IA.IArray IA.Array Color) => Int -> SimplePixelOp
arbitraryGaussianFilterPixels n pxs = applyFilterPixels (createGaussianFilter n) pxs

-- Applies a 5 x 5 Gaussian filter to the image.
gaussianFilterPixels :: (IA.IArray IA.Array Color) => SimplePixelOp
gaussianFilterPixels pxs 
  = arbitraryGaussianFilterPixels 5 pxs

-- This creates a filter that represents the entire image (i.e. convolving it
-- with any image will yield the same image). It consists of 0s and then a 1
-- at the center.
createImageFilter :: (Fractional a, Enum a) => Int -> Matrix a
createImageFilter n = toMatrix n n (zs ++ [1] ++ zs)
 where
  m = (n^2) `div` 2
  zs = replicate m 0

-- Combines two filters using the operation passed in (addition or subtraction).
combineFilters :: (Fractional a, Enum a) 
               => (a -> a -> a) 
               -> Matrix a 
               -> Matrix a 
               -> Matrix a
combineFilters op filt_x filt_y = toMatrix r c $ zipWith op (fromMatrix filt_x) (fromMatrix filt_y)
 where
  (r, c) = getRowCol filt_x

-- Creates an edge detection filter by taking the image filter and subtracting
-- a low pass filter from it.  
createEdgeDetectFilter :: (Fractional a, Enum a)
                       => (Int -> Matrix a)
                       -> Int
                       -> Matrix a
createEdgeDetectFilter createLowpassFilter n 
  = combineFilters (-) (createImageFilter n) (createLowpassFilter n)

-- Creates an edge enhancemenet filter by adding the image back to a filter that's
-- used for edge detection.
createEdgeEnhanceFilter :: (Fractional a, Enum a)
                        => (Int -> Matrix a)
                        -> Int
                        -> Matrix a 
createEdgeEnhanceFilter createLowpassFilter n
  = combineFilters (+) (createImageFilter n) (createEdgeDetectFilter createLowpassFilter n)

edgeDetectPixels :: (IA.IArray IA.Array Color) => SimplePixelOp
edgeDetectPixels pxs 
  = applyFilterPixels (createEdgeDetectFilter createGaussianFilter 5) pxs

edgeEnhancePixels :: (IA.IArray IA.Array Color) => SimplePixelOp
edgeEnhancePixels pxs 
  = applyFilterPixels (createEdgeEnhanceFilter createGaussianFilter 5) pxs

