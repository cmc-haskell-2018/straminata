-- | Contains commonly used functions.
module Util.Common where

import Model.CommonTypes
import Visual.WindowConstants
import Util.Controls

frameWidth :: Float
frameWidth = 25

gravitationalConstant :: Float
gravitationalConstant = 9.8

gravitationalVector :: Vector
gravitationalVector = Vector (0, - gravitationalConstant)

frictionCoef :: Float
frictionCoef = 10


-- | Check if two hitboxes collide.
objectsCollide :: Object -> Object -> Bool
objectsCollide h1 h2 = let boxes1 = objectCollisionBoxes h1
                           boxes2 = objectCollisionBoxes h2
                           offset1 = objectPosition h1
                           offset2 = objectPosition h2
                        in any (\box1 ->
                                 any (\box2 ->
                                       collide
                                       (offsetRectangle offset1 box1)
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


objectCollideWithTile :: Object -> Tile -> Bool
objectCollideWithTile _ (Transparent _) = False
objectCollideWithTile o (Solid app) =
  let boxes = objectCollisionBoxes o
      offset = objectPosition o
  in any (\box ->  collide (offsetRectangle offset box) (appearanceBox app)) boxes

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
updateObjects game = foldr (\obj -> objectOnUpdate obj $ obj) game (gameObjects game)


updatePhysics :: Float -> Game -> Game
updatePhysics time = moveObjects time . movePlayers time . updateVelocities time . updateAccelerations time . nullifyAccelerations


objectOnGround :: Object -> Map -> Bool
objectOnGround object tiles = any (objectCollideWithTile object) (concat tiles)


frictionVector :: Object -> Vector
frictionVector obj = takeShortest newVector (invertVector . objectVelocity $ obj)--(invertVector $ projectVector (objectAcceleration obj) velDir)
  where velDir = normalizeVector . objectVelocity $ obj
        newVector = invertVector velDir `mulByNumber` (frictionCoef * gravitationalConstant)

reactVector :: Object -> Vector
reactVector obj = frictionVector obj `plus` invertVector gravitationalVector


updateVelocities :: Float -> Game -> Game
updateVelocities time game =
  game { gamePlayers = map (performControl . newPlayer) (gamePlayers game)
       , gameLevel = (gameLevel game) {
           levelObjects = map newObject (levelObjects . gameLevel $ game)
         }
       }
  where newVelocity acc vel = performMove time acc vel
        newObject object =
          object { objectVelocity = positionToVector $ newVelocity (objectAcceleration object) (vectorToPosition . objectVelocity $ object)
                 }
        newPlayer player =
          player { playerObject = newObject (playerObject player)
                 }
        performControl player =
          player { playerObject = addControlVector (playerObject $ player) (playerControlVector player)
                 }


addControlVector :: Object -> Vector -> Object
addControlVector obj con =
  let vel = objectVelocity obj
      proj = projectVector vel con
      angle = angleCosBetweenVectors con vel
      newVelocity = if angle < 0
                    then vel `plus` con
                    else if (vectorLength proj) > (vectorLength con)
                         then vel
                         else vel `plus` (con `subtractVector` proj)
  in obj { objectVelocity = newVelocity
         }


updateAccelerations :: Float -> Game -> Game
updateAccelerations _ game =
  game { gamePlayers = map acceleratePlayer (gamePlayers game)
       , gameLevel = (gameLevel game) {
           levelObjects = map accelerateObject (levelObjects . gameLevel $ game)
         }
       }
  where
--   addControlElement player =
--           player { playerObject = (playerObject player)
--                      { objectAcceleration = (objectAcceleration . playerObject $ player) `plus` playerControlVector player
--                      }
--                  }
        acceleratePlayer player =
          player { playerObject = accelerateObject (playerObject player)
                 }
        accelerateObject object = addReactElement . addGravitationalElement $ object
        addGravitationalElement object = --object
          object { objectAcceleration = objectAcceleration object `plus` gravitationalVector
                 }
        addReactElement object =
          if objectOnGround object (levelMap . gameLevel $ game)
          then object { objectAcceleration = objectAcceleration object `plus` reactVector object
                      }
          else object
--           object { objectAcceleration = objectAcceleration object `plus` reactVector object
--                  }


nullifyAccelerations :: Game -> Game
nullifyAccelerations game =
  game { gamePlayers = map nullPlayerAcceleration (gamePlayers game)
       , gameLevel = (gameLevel game) {
           levelObjects = map nullAcceleration (levelObjects . gameLevel $ game)
         }
       }
  where nullAcceleration object =
          object { objectAcceleration = zeroVector
                 }
        nullPlayerAcceleration player =
          player { playerObject = nullAcceleration (playerObject player)
                 }



movePlayer :: Vector -> Action
movePlayer vector =
  PlayerAction (\player ->
    let object = playerObject player
        currentVelocity = objectVelocity object
    in player { playerObject = object { objectVelocity = vector `plus` currentVelocity } }
  )

setTextureByName :: String -> Texture -> Action
setTextureByName name texture =
  GameAction (\_ game ->
    game { gamePlayers =
             map (\player -> if isTarget player
                             then player {
                                    playerObject = (playerObject player) {
                                      objectAppearance = (objectAppearance . playerObject $ player) {
                                        appearanceActualSize = fst texture
                                      , appearancePicture = snd texture
                                      }
                                    }
                                  }
                             else player
             ) (gamePlayers game)
         }
  )
  where isTarget player = (objectName . playerObject $ player) == name