module Controls where

import Graphics.Gloss.Interface.IO.Game hiding (Vector)

import Model.CommonTypes

-- | Generates next game state based on user input.
handleInput :: Event -> Game -> Game
handleInput event game = game { gamePlayers = map (movePlayer event) (gamePlayers game) }

movePlayer :: Event -> Player -> Player
movePlayer event player = (playerControls player) player event

changeVelocityOnCopy :: Vector
                     -> Player
                     -> Player
changeVelocityOnCopy velocity player =
  let object = playerObject player
      currentVelocity = objectVelocity object
  in player { playerObject = object { objectVelocity = velocity `plus` currentVelocity } }

moveObjects :: Time -> Game -> Game
moveObjects time game = game { gameObjects = move $ gameObjects game }
  where move = map (moveObject time)

movePlayers :: Time -> Game -> Game
movePlayers time game = game
  { gamePlayers = map (\player -> player { playerObject = moveObject time (playerObject player) }) (gamePlayers game)
  }


moveObject :: Time -> Object -> Object
moveObject time obj = let h = objectHitbox obj
                          v = objectVelocity obj
                          pos = hitboxPosition h
                      in obj { objectHitbox = h { hitboxPosition = performMove time v pos } }

standardVelocity :: Float
standardVelocity = 30

performMove :: Time -> Vector -> Position -> Position
performMove time (Vector (vx, vy)) (Position (x, y)) = Position ( x + vx * time * standardVelocity
                                                                , y + vy * time * standardVelocity
                                                                )
