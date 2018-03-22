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

bindAction :: Key -> Action -> Action -> KeyPress
bindAction key up down = (key, up, down)

upOrDown :: KeyState -> KeyPress -> Action
upOrDown Down (_, x, _) = x
upOrDown Up (_, _, x) = x

