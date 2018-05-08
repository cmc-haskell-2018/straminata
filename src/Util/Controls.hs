module Util.Controls where

import Data.Maybe (isJust, fromJust)
import Data.List (find)

import Graphics.Gloss.Interface.IO.Game hiding (Vector)

import Model.CommonTypes

-- | Generates next game state based on user input.
handleInput :: Event -> Game -> Game
handleInput event game =
  let players = gamePlayers game
  in case event of
    (EventKey (MouseButton LeftButton) Down _ point) -> downOk game point
    (EventKey (Char 'o') _ _ _) -> game { gameLevel = (gameLevel game) { levelStart = False} 
                                        , gameRules = False
                                        }
    (EventKey (Char 'r') _ _ _) -> game { gameRules = True }
    (EventKey key state _ _) -> foldr (actOnGame key state)
                                      game { gamePlayers = map (actOnPlayer key state game) players
                                           }
                                      players
    _ -> game

    where actOnPlayer key state game' player = let action = findAction key state player
                                         in if isJust action
                                            then performOnPlayer (fromJust action) player game'
                                            else player
          actOnGame key state player game' = let action = findAction key state player
                                            in if isJust action
                                               then performOnGame (fromJust action) player game'
                                               else game'
          findAction key state player = (upOrDown state) <$> find (predicate key) (playerControls player)
          predicate key (key', _, _) = key == key'

downOk :: Game -> Point -> Game
downOk game (x, y) 
  | x >= 100 && x <= 220  && y <= -100 && y >= -160 && rules == True = game { gameRules = False }
  | x >= 100 && x <= 220  && y <= -100 && y >= -160 && start == True =  game { gameLevel = (gameLevel game) 
                                                                                  { levelStart = False }
                                                                             }
  | otherwise = game
  where start = levelStart . gameLevel $ game
        rules = gameRules game

bindAction :: Key -> Action -> Action -> KeyPress
bindAction key up down = (key, up, down)

upOrDown :: KeyState -> KeyPress -> Action
upOrDown Down (_, x, _) = x
upOrDown Up (_, _, x) = x

