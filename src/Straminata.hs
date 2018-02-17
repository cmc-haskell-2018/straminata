{-# OPTIONS_GHC -Wall #-}
-- | __Straminata__ is a puzzle platform game written completely in Haskell.
module Straminata where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event)

import WindowConstants
import Model.CommonTypes
import Visual.Renderer

-- | Starts game main loop.
run :: IO ()
run = play
      newWindow
      backgroundColor
      stepsPerSecond
      initialWorld
      render
      handleInput
      advanceGame

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
                            , hitbox = Hitbox { position = Position (0, 0)
                                              , scaling = Scaling (1, 1)
                                              , boxes = [Rectangle (Position (0, 0), Position (30, 50))]
                                              }
                            , velocity = Vector (0, 0)
                            }

-- | Generates next game state based on user input.
handleInput :: Event -> Game -> Game
handleInput _ _ = initialWorld


-- | Advances game state one step further.
advanceGame :: Float -- ^ period of time (in seconds) needing to be advanced
            -> Game
            -> Game
advanceGame _ _ = initialWorld
