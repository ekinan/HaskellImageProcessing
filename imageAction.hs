{-#
  OPTIONS_GHC -XTypeSynonymInstances 
              -XFlexibleInstances
              -XMultiParamTypeClasses
#-}
module ImageAction where
-- Library imports
import Graphics.UI.WX
import Graphics.UI.WXCore
import Data.Maybe 

{-
Here is our ImageAction type class, used to make the menu items polymorphic
(and easy to extend). Note that an image action is anything that modifies
the input image to generate a new output image, and updates the frame accordingly.
We have three possible actions so far:
  1. Basic image operations (pixel to a new set of pixels)
  2. Random image operations (random dithering)
  3. User-driven image operations (e.g. applying an arbitrary Gaussian filter).
-}

-- Custom file imports
import Utilities

class ImageAction a where
  doActionOnImage :: Frame b -> Var (Image ()) -> Var (Image ()) -> a -> IO ()

instance ImageAction ImageOp where
  doActionOnImage f vInpImg vOutImg imgOp = applyImageOp f vInpImg vOutImg imgOp

instance ImageAction SimplePixelOp where
  doActionOnImage f vInpImg vOutImg pxOp = doActionOnImage f vInpImg vOutImg (SimpleImageOp pxOp)

instance ImageAction RandPixelOp where
  doActionOnImage f vInpImg vOutImg randPxOp 
    = doActionOnImage f vInpImg vOutImg (RandomImageOp randPxOp)

-- TODO: Is there a way to make it (Frame b -> IO (a), a -> SimplePixelOp) ?
instance ImageAction (IO (a), a -> SimplePixelOp) where
  doActionOnImage f vInpImg vOutImg (inpGen, inpPxOp)
    = do inp <- inpGen 
         applyImageOp f vInpImg vOutImg (SimpleImageOp $ inpPxOp inp)
