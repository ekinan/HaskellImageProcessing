# HaskellImageProcessing
This is a basic image processing GUI I made during my independent study in the Fall 2016 term using wxHaskell. It is menu-driven. First, the user loads an image from their computer; currently the accepted format is TAR. Afterwards, they can select each of the menu panes in the GUI representing some type of operation, and then expand those to select the exact operation that they want to perform. Currently, the following operations are available:
	1. Recolor
		-Not a very creative name, but basically the user can convert their image to 
		grayscale, do a basic uniform quantization on it, or they can do populosity
		quantization.

	2. Dither
		-Has threshold, cluster, constant brightness, and Floyd-Steinberg (grayscale
		and color) dithering.

	3. Filter
		-Apply a box filter, Gaussian filter, and edge detection/enhancement filter.

Unfortunately I wasn't able to get the "Save Image" part working. This project was mostly intended to get me more comfortable with some of the higher-order functions provided by Haskell and to use the language in a more real-world setting.
