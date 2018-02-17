{-# OPTIONS_GHC -Wall #-}
-- | __Straminata__ is a puzzle platform game written completely in Haskell.
module Straminata where

import Graphics.Gloss

import WindowConstants
import Model.CommonTypes
import Visual.Renderer
import Util.Common
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
                    , players = Players { firstPlayer = playerInitialState, secondPlayer = player2InitialState }
                    }

-- todo: get from file
initialObjects :: [Object]
initialObjects = []

-- todo: get from file
playerInitialState :: Player
playerInitialState = Player { object = Object { name = "Player 1"
                                              , hitbox = Hitbox { position = Position (0, 0)
                                                                , displayBox = Rectangle (Position (0, 0), Position (100, 100))
                                                                , collisionBoxes = [Rectangle (Position (0, 0), Position (100, 100))]
                                                                }
                                              , velocity = Vector (0, 0)
                                              }
                            , playerColor = red
                            }

player2InitialState :: Player
player2InitialState = Player { object = Object { name = "Player 2"
                                              , hitbox = Hitbox { position = Position (0, 0)
                                                                , displayBox = Rectangle (Position (0, 0), Position (50, 50))
                                                                , collisionBoxes = [Rectangle (Position (0, 0), Position (50, 50))]
                                                                }
                                              , velocity = Vector (0, 0)
                                              }
                            , playerColor = red
                            }


-- | Advances game state one step further.
advanceGame :: Float -- ^ period of time (in seconds) needing to be advanced
            -> Game
            -> Game
advanceGame time = updatePlayers . moveObjects time . movePlayers time

-- temporary
updatePlayers :: Game -> Game
updatePlayers game = let playersList = players game
                         player1 = firstPlayer playersList
                         player2 = secondPlayer playersList
                         h1 = hitbox . object $ player1
                         h2 = hitbox . object $ player2
                         changeColor = hitboxesCollide h1 h2
                     in game { players = playersList { firstPlayer = player1 { playerColor = if changeColor then red else blue }
                                                     , secondPlayer = player2 { playerColor = if changeColor then green else blue }
                                                     }
                             }
