{-#
  OPTIONS_GHC -XFlexibleContexts
#-}
module Main where
-- Library imports
import Data.Maybe
import Graphics.UI.WX
import Graphics.UI.WXCore

{-
This is the main program. Once WXHaskell installs, along with the necessary
dependencies (nothing serious, only requires one cabal install for each
dependency), this program is ready to go.
-}

-- Custom file imports
import MenuBars
import Utilities

main = start gui

-- Sets up the GUI to use with WXHaskell
gui :: IO ()
gui =
  do 
     f            <- frame     [text := "Image Manipulation GUI"]
     
     -- Creates the images
     bImg         <- allBlackIO
     vInpImg      <- variable [value := bImg]
     vOutImg      <- variable [value := bImg]

     inpPanelInfo <- createImagePanel f vInpImg "Input Image"
     outPanelInfo <- createImagePanel f vOutImg "Output Image"

     -- Sets up the layout
     set f [layout := row 5 [(createImagePanelLayout inpPanelInfo)
                            ,(createImagePanelLayout outPanelInfo)]
           ,clientSize := sz 900 600]

     createMenuBars f vInpImg vOutImg

     -- Draws the initial frame
     repaint f
 where
  drawImageWrapper vImg dc view
    = do mImg <- get vImg value
         drawImage dc mImg (point 0 0) []

  createImagePanel f vImg msg
    = do p        <- panel f []
         pLabel   <- staticText p [text := msg
                                  ,fontSize := 14
                                  ,fontWeight := WeightBold]
         imgPanel <- panel p [on paint := drawImageWrapper vImg]
         return (p, pLabel, imgPanel)

  createImagePanelLayout (p, pLabel, imgPanel)
    = container p $ column 10 [hfloatCenter $ widget pLabel
                              ,container imgPanel $ glue]
