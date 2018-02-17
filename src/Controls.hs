module Controls where

import Graphics.Gloss.Interface.IO.Game hiding (Vector)

import Model.CommonTypes

-- | Generates next game state based on user input.
handleInput :: Event -> Game -> Game
handleInput event game = game { players = (players game)
                                          { firstPlayer = movePlayer (firstPlayer . players $ game) event }
                              }

movePlayer :: Player -> Event -> Player
movePlayer player event = let playerObject = object player
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
  _ -> player

changePlayerVelocity :: Vector -- ^ Velocity change.
                     -> Vector -- ^ Player velocity
                     -> Vector
changePlayerVelocity (Vector (cx, cy)) (Vector (x, y)) = Vector (x + cx, y + cy)


moveObjects :: Time -> Game -> Game
moveObjects time game = game { objects = move $ objects game }
  where move = map (moveObject time)

movePlayers :: Time -> Game -> Game
movePlayers time game = let playersList = players game
                            player1 = firstPlayer playersList
                            player2 = secondPlayer playersList
                            obj1 = object player1
                            obj2 = object player2
                        in game { players = (players game) { firstPlayer = player1 { object = moveObject time obj1 }
                                                           , secondPlayer = player2 { object = moveObject time obj2 }
                                                           }
                                }


moveObject :: Time -> Object -> Object
moveObject time obj = let h = hitbox obj
                          v = velocity obj
                          pos = position h
                      in obj { hitbox = h { position = performMove time v pos } }

standardVelocity :: Float
standardVelocity = 30

performMove :: Time -> Vector -> Position -> Position
performMove time (Vector (vx, vy)) (Position (x, y)) = Position ( x + vx * time * standardVelocity
                                                                , y + vy * time * standardVelocity
                                                                )
