{-#
  OPTIONS_GHC -XFlexibleContexts
#-}
module Utilities where
-- Library imports
import Graphics.UI.WX
import Graphics.UI.WXCore
import Data.Maybe
import Data.Array.IArray
import Data.List
import System.Random

{-
This sourcefile contains some helpful utility functions commonly shared throughout
the image processing modules.
-}

-- typedefs
type Matrix a = Array Point a
type ColorPixels = Matrix Color
type Grayscale = Matrix Int

type SimplePixelOp = ColorPixels -> ColorPixels
type RandPixelOp = ColorPixels -> IO (ColorPixels) -- For operations that use rand. #s 

-- Color values can be negative and floating point sometimes,
-- this is a helpful way of accounting for that (makes things like
-- Floyd Steinberg Dithering a lot easier to do.)
type ColorRGB = (Double, Double, Double)

-- Helpful data types

data ImageOp = SimpleImageOp SimplePixelOp 
             | RandomImageOp RandPixelOp 

{-
Applies an arbitrary image op to the input image and stores it in the
output image. Updates the frame f afterwards of the change.
-}
applyImageOp :: Frame a 
             -> Var (Image ()) 
             -> Var (Image ()) 
             -> ImageOp
             -> IO ()
applyImageOp f vInpImg vOutImg imgOp
  = do inpImg <- get vInpImg value
       inpPxs <- imageGetPixelArray inpImg
       outImg <- doImgOpOnPxs imgOp inpPxs
       set vOutImg [value := outImg]
       repaint f
 where
  doImgOpOnPxs (SimpleImageOp pxOp) inpPxs
    = imageCreateFromPixelArray $ pxOp inpPxs
  doImgOpOnPxs (RandomImageOp randPxOp) inpPxs
    = do outPxs <- randPxOp inpPxs
         imageCreateFromPixelArray outPxs

-- Helpful constants
globalGenIO = getStdGen 

-- All black image, used as the default output image when loading a new
-- image.
allBlackIO =
  imageCreateFromPixels (sz 450 400) (replicate 180000 $ rgb 0 0 0)

-- Convenient helper methods
instance Ord Color where
  compare cl1 cl2 = compare (toRGB cl1) (toRGB cl2)

-- Returns the square of the Euclidian distance between
-- two given colors
euclidianDistanceSq :: Color -> Color -> Int
euclidianDistanceSq cl1 cl2 = rdiff^2 + gdiff^2 + bdiff^2
 where
  rdiff = colorRed cl1 - colorRed cl2
  gdiff = colorGreen cl1 - colorGreen cl2
  bdiff = colorBlue cl1 - colorBlue cl2

-- Some helpful Matrix functions

-- Note that it returns (r, c), where r = row size, c = col size
getRowCol :: Matrix a -> (Int, Int)
getRowCol m = (ye+1, xe+1)
 where
  Point xe ye = snd $ bounds m

-- Checks if a point is inside the given matrix
isInsideMatrix :: Matrix a -> Point -> Bool
isInsideMatrix m pt = inRange (bounds m) pt

-- Takes the row and columns of the matrix, the list of elements
-- and returns a matrix
toMatrix :: Int -> Int -> [a] -> Matrix a
toMatrix r c xs = listArray (point 0 0, point (c-1) (r-1)) xs

-- Flattens a matrix
fromMatrix :: Matrix a -> [a]
fromMatrix m = elems m

-- Basic arithmetic operations on a matrix

-- Converts a color to its grayscale value
toGrayVal :: Color -> Int
toGrayVal clr = floor (0.299*red + 0.587*green + 0.114*blue)
 where
  red = colorRed clr
  green = colorGreen clr
  blue = colorBlue clr

-- Takes a gray scale value and converts it to a color
fromGrayVal :: Int -> Color
fromGrayVal v = rgb v v v

-- Converts a given set of pixels to their corresponding grayscale values
toGrayscale :: (IArray Array Color) => ColorPixels -> Grayscale
toGrayscale pxs = amap toGrayVal pxs

-- Conversion functions from normal Color to ColorRGB (i.e. Int values to
-- floating point ones.)
toRGB :: Color -> ColorRGB
toRGB cl 
  = (fromIntegral $ colorRed cl, fromIntegral $ colorGreen cl, fromIntegral $ colorBlue cl)

fromRGB :: ColorRGB -> Color
fromRGB (r, g, b) = rgb (floor r) (floor g) (floor b)
