module Controls where

import Graphics.Gloss.Interface.IO.Game hiding (Vector)

import Model.CommonTypes

-- | Generates next game state based on user input.
handleInput :: Event -> Game -> Game
handleInput event game = game { players = map (movePlayer event) (players game) }

movePlayer :: Event -> Player -> Player
movePlayer event player = (playerControls player) player event

changePlayerVelocity :: Vector -- ^ Velocity change.
                     -> Vector -- ^ Player velocity
                     -> Vector
changePlayerVelocity (Vector (cx, cy)) (Vector (x, y)) = Vector (x + cx, y + cy)


moveObjects :: Time -> Game -> Game
moveObjects time game = game { objects = move $ objects game }
  where move = map (moveObject time)

movePlayers :: Time -> Game -> Game
movePlayers time game = game { players = map (\player -> player {object = moveObject time (object player) }) (players game) }


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
