-- |This module keeps default parameters describing the game window
module WindowConstants where

import Graphics.Gloss

windowName :: String
windowName = "s t r a m i n a t a  p r o j e c t"

initialWindowDimensions :: (Int, Int)
initialWindowDimensions = (800, 600)

initialWindowPosition :: (Int, Int)
initialWindowPosition = (10, 10)

backgroundColor :: Color
backgroundColor = white

stepsPerSecond :: Int
stepsPerSecond = 30
