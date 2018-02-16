{-# OPTIONS_GHC -Wall #-}
-- | __Straminata__ is a puzzle platform game written completely in Haskell.
module Straminata where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event)

import WindowConstants

-- | Represents a game session.
data Game = Game {
                 }

-- | Starts game main loop.
run :: IO ()
run = play
      window
      backgroundColor
      stepsPerSecond
      initialWorld
      render
      handleInput
      advanceGame

-- | Initial window state.
window :: Display
window = InWindow windowName initialWindowDimensions initialWindowPosition

-- | Initial game state.
initialWorld :: Game
initialWorld = Game


-- | Performs scene rendering inside a window.
render :: Game -> Picture
render = const picture


-- | Generates next game state based on user input.
handleInput :: Event -> Game -> Game
handleInput _ _ = Game


-- | Advances game state one step further.
advanceGame :: Float -- ^ period of time (in seconds) needing to be advanced
            -> Game
            -> Game
advanceGame _ _ = Game


picture :: Picture
picture = Translate (-170) 0 $ Scale 0.5 0.5 $ Text "Hello World"
