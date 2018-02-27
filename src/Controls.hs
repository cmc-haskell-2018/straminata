module Controls where

import Data.Maybe (isJust, fromJust)
import Data.List (find)
import Data.Char (toLower)

import Graphics.Gloss.Interface.IO.Game hiding (Vector)

import Model.CommonTypes

-- | Generates next game state based on user input.
handleInput :: Event -> Game -> Game
handleInput event game =
  let players = gamePlayers game
  in case event of
    (EventKey key state _ _) -> game { gamePlayers = map (act key state) players }
    _ -> game
    where act key state player = let controlElement = (upOrDown state) <$> find (predicate key) (playerControls player)
                                 in if isJust controlElement
                                    then controlPlayer (fromJust controlElement) player
                                    else player
          predicate (Char key) ((Char key'), _, _) = (toLower key) == key'
          predicate key (key', _, _) = key == key'

bindAction :: Key -> ControlElement -> ControlElement -> KeyPress
bindAction key up down = (key, up, down)

moveObjects :: Time -> Game -> Game
moveObjects time game = game { gameObjects = move $ gameObjects game }
  where move = map (moveObject time)

movePlayers :: Time -> Game -> Game
movePlayers time game = game
  { gamePlayers = map (\player -> player { playerObject = moveObject time (playerObject player) }) (gamePlayers game)
  }


moveObject :: Time -> Object -> Object
moveObject time obj = let v = objectVelocity obj
                          pos = objectPosition obj
                      in obj { objectPosition = performMove time v pos }

standardVelocity :: Float
standardVelocity = 30

performMove :: Time -> Vector -> Position -> Position
performMove time (Vector (vx, vy)) (Position (x, y)) = Position ( x + vx * time * standardVelocity
                                                                , y + vy * time * standardVelocity
                                                                )
