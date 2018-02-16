{-# OPTIONS_GHC -Wall #-}
-- | __Straminata__ is a puzzle platform game written completely in Haskell.
module Straminata where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event)

import WindowConstants
import Model.CommonTypes
import Util.Common

-- | Represents a game session.
data Game = Game { objects :: [Object]
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
initialWorld = Game { objects = initialObjects
                    }

-- todo: get from file
initialObjects :: [Object]
initialObjects = [ playerInitialState
                 ]

-- todo: get from file
playerInitialState :: Object
playerInitialState = Object { name = "Player 1"
                            , position = Position (0, 0)
                            , dimensions = Dimensions (15, 25)
                            , scaling = Scaling (1, 1)
                            , velocity = Vector (0, 0)
                            }

-- | Performs scene rendering inside a window.
render :: Game -> Picture
render = picture


-- | Generates next game state based on user input.
handleInput :: Event -> Game -> Game
handleInput _ _ = initialWorld


-- | Advances game state one step further.
advanceGame :: Float -- ^ period of time (in seconds) needing to be advanced
            -> Game
            -> Game
advanceGame _ _ = initialWorld


-- | Composes all game @objects@ in a list of pictures.
picture :: Game -> Picture
picture game = Pictures $ map (\object ->
                               uncurry Translate (unwrap $ position object)
                               $ uncurry Scale (unwrap $ scaling object)
                               $ Polygon (formPath (position object) (dimensions object)))
                        $ objects game
