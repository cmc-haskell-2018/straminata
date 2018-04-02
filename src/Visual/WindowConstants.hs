-- |This module keeps default parameters describing the game window
module Visual.WindowConstants where

import Graphics.Gloss

windowName :: String
windowName = "STRAMiiNATA PR0JECT"

initialWindowDimensions :: (Int, Int)
initialWindowDimensions = (1200, 800)

initialWindowPosition :: (Int, Int)
initialWindowPosition = (250, 150)

backgroundColor :: Color
backgroundColor = blue

fps :: Int
fps = 60
