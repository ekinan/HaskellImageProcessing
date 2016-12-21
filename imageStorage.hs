module ImageStorage where

-- Library imports
import Graphics.UI.WX
import Graphics.UI.WXCore

-- Custome file imports
import Utilities

-- These are the kinds of images that our software allows.

{-
This source file contains our allowable images, and also functions to
generate the load and save menus.
-}

allowableFiles =
  [
    ("TGA Images", ["*.tga"])
  ]

loadImage :: Frame a -> Var (Image ()) -> IO ()
loadImage f vImg =
  do mFilePath <- fileOpenDialog f True True "Load Image" allowableFiles "" "" 
     case mFilePath of
       Nothing       -> do bImg <- allBlackIO
                           set vImg [value := bImg]

       Just filePath -> do img <- imageCreateFromFile filePath
                           set vImg [value := img]

     repaint f


-- TODO: Figure out how this works!
saveImage :: Frame a -> Var (Image ()) -> IO ()
saveImage f vImg =
  do fileSaveDialog f True True "Save Image" allowableFiles "" "output.tga"
     return ()
