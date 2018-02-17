{-# OPTIONS_GHC -Wall #-}
-- | __Straminata__ is a puzzle platform game written completely in Haskell.
module Straminata where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game hiding (Vector)

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
                            , playerControls
  = (\player event
     -> let playerObject = object player
            playerVelocity = velocity playerObject
        in case event of
        (EventKey (SpecialKey KeyRight) Down _ _)
          -> player { object = playerObject { velocity = changePlayerVelocity (Vector (1, 0)) playerVelocity } }
        (EventKey (SpecialKey KeyRight) Up _ _)
          -> player { object = playerObject { velocity = changePlayerVelocity (Vector (-1, 0)) playerVelocity } }
        (EventKey (SpecialKey KeyLeft) Down _ _)
          -> player { object = playerObject { velocity = changePlayerVelocity (Vector (-1, 0)) playerVelocity } }
        (EventKey (SpecialKey KeyLeft) Up _ _)
          -> player { object = playerObject { velocity = changePlayerVelocity (Vector (1, 0)) playerVelocity } }
        (EventKey (SpecialKey KeyUp) Down _ _)
          -> player { object = playerObject { velocity = changePlayerVelocity (Vector (0, 1)) playerVelocity } }
        (EventKey (SpecialKey KeyUp) Up _ _)
          -> player { object = playerObject { velocity = changePlayerVelocity (Vector (0, -1)) playerVelocity } }
        (EventKey (SpecialKey KeyDown) Down _ _)
          -> player { object = playerObject { velocity = changePlayerVelocity (Vector (0, -1)) playerVelocity } }
        (EventKey (SpecialKey KeyDown) Up _ _)
          -> player { object = playerObject { velocity = changePlayerVelocity (Vector (0, 1)) playerVelocity } }
        _ -> player )
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
                            , playerControls
  = (\player event
     -> let playerObject = object player
            playerVelocity = velocity playerObject
        in case event of
        (EventKey (Char 'd') Down _ _)
          -> player { object = playerObject { velocity = changePlayerVelocity (Vector (1, 0)) playerVelocity } }
        (EventKey (Char 'd') Up _ _)
          -> player { object = playerObject { velocity = changePlayerVelocity (Vector (-1, 0)) playerVelocity } }
        (EventKey (Char 'a') Down _ _)
          -> player { object = playerObject { velocity = changePlayerVelocity (Vector (-1, 0)) playerVelocity } }
        (EventKey (Char 'a') Up _ _)
          -> player { object = playerObject { velocity = changePlayerVelocity (Vector (1, 0)) playerVelocity } }
        (EventKey (Char 'w') Down _ _)
          -> player { object = playerObject { velocity = changePlayerVelocity (Vector (0, 1)) playerVelocity } }
        (EventKey (Char 'w') Up _ _)
          -> player { object = playerObject { velocity = changePlayerVelocity (Vector (0, -1)) playerVelocity } }
        (EventKey (Char 's') Down _ _)
          -> player { object = playerObject { velocity = changePlayerVelocity (Vector (0, -1)) playerVelocity } }
        (EventKey (Char 's') Up _ _)
          -> player { object = playerObject { velocity = changePlayerVelocity (Vector (0, 1)) playerVelocity } }
        _ -> player )
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
