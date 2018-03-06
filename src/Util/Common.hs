-- | Contains commonly used functions.
module Util.Common where

import Model.CommonTypes
import Visual.WindowConstants
import Data.Foldable
import Data.Maybe

frameWidth :: Float
frameWidth = 25

-- | Check if two hitboxes collide.
objectsCollide :: Object -> Object -> Bool
objectsCollide h1 h2 = let boxes1 = objectCollisionBoxes h1
                           boxes2 = objectCollisionBoxes h2
                           offset1 = objectPosition h1
                           offset2 = objectPosition h2
                        in any (\box1 -> any (\box2 -> collide (offsetRectangle offset1 box1)
                                                               (offsetRectangle offset2 box2)
                                             ) boxes2
                               ) boxes1

-- | Returns Rectangle which coordinates are offsetted by Position.
offsetRectangle :: Position -> Rectangle -> Rectangle
offsetRectangle (Position (x, y))
                (Position (x1, y1), Position (x2, y2)) = (Position (x1 + x, y1 + y), Position (x2 + x, y2 + y))

-- | Checks if two rectangles are colliding.
collide :: Rectangle -> Rectangle -> Bool
collide (Position (x11, y11), Position (x12, y12))
        (Position (x21, y21), Position (x22, y22)) = x11 < x22 && x12 > x21 && y11 < y22 && y12 > y21


updateCamera :: Game -> Game
updateCamera game = game
  { gameCamera = (gameCamera game)
    { cameraPosition = vectorToPosition position
    , cameraRatio = ratio
    }
  }
  where
    appearanceBoxes = map (offsetAppearanceBox . playerObject) (gamePlayers game)
    offsetAppearanceBox = \object -> offsetRectangle (objectPosition $ object) (appearanceBox . objectAppearance $ object)
    position = (Vector (rect !! 0, rect !! 1) `plus` Vector (rect !! 2, rect !! 3)) `divByNumber` 2
    rect = [ minimum (map (fst . unwrap . fst) appearanceBoxes) - frameWidth
           , minimum (map (snd . unwrap . fst) appearanceBoxes) - frameWidth
           , maximum (map (fst . unwrap . snd) appearanceBoxes) + frameWidth
           , maximum (map (snd . unwrap . snd) appearanceBoxes) + frameWidth
           ]
    boundaryDimensions = (rect !! 2 - rect !! 0, rect !! 3 - rect !! 1)
    ratio = minimum [ (fromIntegral . fst $ initialWindowDimensions) / (fst boundaryDimensions)
                    , (fromIntegral . snd $ initialWindowDimensions) / (snd boundaryDimensions)
                    , 1
                    ]


gameObjects :: Game -> [Object]
gameObjects game = map playerObject (gamePlayers game)
                   ++ levelObjects (gameLevel game)


updateObjects :: Game -> Game
updateObjects game = foldr (\obj -> stringToUpdateFunction (objectOnUpdateName obj) $ obj) game (gameObjects game)

activatePlayer :: String -> Object -> Game -> Game
activatePlayer name object game =
  foldr (\player -> (stringToActivateFunction (objectOnActivateName . playerObject $ player))
                    (objectsCollide object (playerObject player))
                    (playerObject player)
        )
        game
        (filter isTarget (gamePlayers game))
  where isTarget player = (objectName . playerObject $ player) == name

namedUpdateFunctions :: [NamedUpdateFunction]
namedUpdateFunctions = [("onUpdateMario", activatePlayer "luigi")]

namedActivateFunctions :: [NamedActivateFunction]
namedActivateFunctions = [("resizeSelf", resizeSelf)]

stringToUpdateFunction :: String -> Object -> Game -> Game
stringToUpdateFunction name = let namedFunction = find (\(x,_) -> x == name) namedUpdateFunctions
                              in if isJust namedFunction
                                 then snd (fromJust namedFunction) 
                                 else (\_ -> id) 

stringToActivateFunction :: String -> Bool -> Object -> Game -> Game
stringToActivateFunction name = let namedFunction = find (\(x,_) -> x == name) namedActivateFunctions
                                in if isJust namedFunction
                                   then snd (fromJust namedFunction) 
                                   else (\_ _ -> id)  

resizeSelf :: Bool -> Object -> Game -> Game
resizeSelf state self game = game
  { gamePlayers = map (\player -> if isSelf player
                                  then if state
                                       then enlarge player
                                       else reduce player
                                  else player
                      ) (gamePlayers game)
  }
  where isSelf player = (objectName . playerObject $ player) == (objectName self)
        enlarge = changeSize (Position (-30, -40), Position (90, 120))
        reduce = changeSize (Position (0, 0), Position (60, 80))
        changeSize rect player = player
          { playerObject = (playerObject player)
            { objectAppearance = (objectAppearance . playerObject $ player)
              { appearanceBox = rect
              }
            , objectCollisionBoxes = [rect]
            }
          }
          