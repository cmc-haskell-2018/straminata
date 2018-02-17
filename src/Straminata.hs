{-# OPTIONS_GHC -Wall #-}
-- | __Straminata__ is a puzzle platform game written completely in Haskell.
module Straminata where

import Graphics.Gloss

import WindowConstants
import Model.CommonTypes
import Visual.Renderer
import Controls

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
                    , players = Players { firstPlayer = playerInitialState, secondPlayer = playerInitialState }
                    }

-- todo: get from file
initialObjects :: [Object]
initialObjects = []

-- todo: get from file
playerInitialState :: Player
playerInitialState = Player { object = Object { name = "Player 1"
                                              , hitbox = Hitbox { position = Position (0, 0)
                                                                , scaling = Scaling (1, 1)
                                                                , boxes = [Rectangle (Position (0, 0), Position (30, 50))]
                                                                }
                                              , velocity = Vector (0, 0)
                                              }
                            , playerInfo = PlayerInfo
                            }


-- | Advances game state one step further.
advanceGame :: Float -- ^ period of time (in seconds) needing to be advanced
            -> Game
            -> Game
advanceGame time = moveObjects time . movePlayers time
