{-#
  OPTIONS_GHC -XExistentialQuantification
#-}
module MenuBars
(
  createMenuBars
)  where
-- Library imports
import Graphics.UI.WX
import Graphics.UI.WXCore
import Data.Maybe
import Control.Monad
import Safe

-- Custom file imports
import ImageStorage
import Recolor
import Dither
import Filter
import Utilities
import ImageAction

{-
This module contains all of the information for the menu items. The idea
is that every menu will have a pane that has a bunch of options for the user
to click on. 
-}


{--
Creates the "Load Image" and "Save Image" features. Unfortunately, the "Save
Image" feature is not quite working :(, but the "Load Image" one is.
--}
createFileMenu :: Frame a -> Var (Image ()) -> Var (Image ()) -> IO (Menu ())
createFileMenu f vInpImg vOutImg =  
  do file <- menuPane [text := "&File"]
     load <- menuItem file [text := "&Load Image"
                           ,on command := loadImage f vInpImg]

     save <- menuItem file [text := "&Save Image"
                           ,on command := saveImage f vOutImg]

     menuQuit file [on command := close f]

     return file

-- Data type representing the kinds of image operations that we can do.
-- This is so that we can have a heterogenous list of image menu item actions
-- that make it easer to add new menu items to our frame if we need to.
data ImageMenuItemAction = forall a . (ImageAction a) => ImageMenuItemAction a

type ImageMenuItem = (String, ImageMenuItemAction)
type ImageMenuPane = (String, [ImageMenuItem])

{-
Creates a menu item that does some image processing on a given input image. The
idea is that every menu item will have some text indicating its title, and then
an associated action to execute when the user clicks on something. It returns
its associated pane to create the next menu item.
-}
createImageOpMenuItem :: Frame a
                      -> Var (Image ())
                      -> Var (Image ())
                      -> Menu () 
                      -> ImageMenuItem
                      -> IO (Menu ())
createImageOpMenuItem f vInpImg vOutImg pane (itText, (ImageMenuItemAction imgAction)) =
  do menuItem pane [text := ("&" ++ itText)
                   ,on command := doActionOnImage f vInpImg vOutImg imgAction]
     return pane

{-
Creates a menu pane. Note that a menu pane consists of text to describe what it is,
and then a list of menu items that the user can choose from, and then adds it
to the existing list of menu panes.
-}
createImageOpMenuPane :: Frame a
                      -> Var (Image ())
                      -> Var (Image ())
                      -> [Menu ()]
                      -> ImageMenuPane
                      -> IO [Menu ()]
createImageOpMenuPane f vInpImg vOutImp pnLst (pnText, pnItems) =
  do pane <- menuPane [text := ("&" ++ pnText)]
     foldM_ (createImageOpMenuItem f vInpImg vOutImp) pane pnItems
     return (pnLst ++ [pane])

{-
Creates a bunch of menu panes, by creating the items for each one.
-}
createImageOpMenuPanes :: Frame a
                       -> Var (Image ())
                       -> Var (Image ())
                       -> [ImageMenuPane]
                       -> IO [Menu ()]
createImageOpMenuPanes f vInpImg vOutImg pns =
 foldM (createImageOpMenuPane f vInpImg vOutImg) [] pns 

{-
These are just our menu panes for the image processing program, along with
the associated image action.
-}
recolorMenu :: ImageMenuPane
recolorMenu = (
                 "Recolor", 
                 [
                    ("To Grayscale", ImageMenuItemAction toGrayPixels),
                    ("Uniform Quantization", ImageMenuItemAction uniformQuantPixels),
                    ("Populosity Quantization", ImageMenuItemAction populosityQuantPixels)
                 ]
              )

ditherMenu :: ImageMenuPane
ditherMenu = (
                 "Dither", 
                 [
                    ("Threshold", ImageMenuItemAction ditherThreshPixels),
                    ("Constant Brightness", ImageMenuItemAction ditherConstBrightPixels),
                    ("Random", ImageMenuItemAction ditherRandomPixels),
                    ("Cluster", ImageMenuItemAction ditherClusterPixels),
                    ("Floyd-Steinberg Grayscale", ImageMenuItemAction ditherFSGrayPixels),
                    ("Floyd-Steinberg Color", ImageMenuItemAction ditherFSColorPixels)
                 ]
              )

filterMenu :: Frame a -> ImageMenuPane
filterMenu f = (
                 "Filter", 
                 [
                    ("Box", ImageMenuItemAction boxFilterPixels),
                    ("Bartlett", ImageMenuItemAction bartlettFilterPixels),
                    (
                       "Arbitrary Gaussian", 
                        ImageMenuItemAction (getFilterSize, arbitraryGaussianFilterPixels)
                    ), 
                    ("Gaussian", ImageMenuItemAction gaussianFilterPixels),
                    ("Edge Detection", ImageMenuItemAction edgeDetectPixels),
                    ("Edge Enhance", ImageMenuItemAction edgeEnhancePixels)
                 ]
               )
 where
  getFilterSize
    = do fltrSzStr <- textDialog f "Enter the size of the filter" 
                                   "Filter Size Input" 
                                   "3"
         return $ case (readMay fltrSzStr) of
                  (Just fltrSz) -> fltrSz
                  (Nothing)     -> (3 :: Int)
       
       
-- These are all of our menu panes
imageOpMenuPanes :: Frame a -> [ImageMenuPane]
imageOpMenuPanes f = [recolorMenu, ditherMenu, filterMenu f]

-- Creates the menu bars for the frame to use.
createMenuBars :: Frame a -> Var (Image ()) -> Var (Image ()) -> IO () 
createMenuBars f vInpImg vOutImg =
  do file       <- createFileMenu f vInpImg vOutImg
     imgOps <- createImageOpMenuPanes f vInpImg vOutImg $ imageOpMenuPanes f

     set f [menuBar := ([file] ++ imgOps)]           
