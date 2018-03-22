-- | Contains commonly used functions.
module Util.Common where

import Model.CommonTypes
import Visual.WindowConstants
import Util.Constants

frameWidth :: Float
frameWidth = 25

gravitationalVector :: Vector
gravitationalVector = Vector (0, - g)

frictionCoef :: Float
frictionCoef = 100

standardVelocity :: Float
standardVelocity = 30


cosToSin :: Float -> Float
cosToSin = sin . acos


-- | Check if two objects collide.
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


translateRectByX :: Float -> Rectangle -> Rectangle
translateRectByX a (Position (x1, y1), Position (x2, y2)) =
  (Position (x1 + a, y1), Position (x2 + a, y2))

translateRectByY :: Float -> Rectangle -> Rectangle
translateRectByY a (Position (x1, y1), Position (x2, y2)) =
  (Position (x1, y1 + a), Position (x2, y2 + a))

-- | Checks if two rectangles are colliding.
collide :: Rectangle -> Rectangle -> Bool
collide (Position (x11, y11), Position (x12, y12))
        (Position (x21, y21), Position (x22, y22)) = x11 < x22 && x12 > x21 && y11 < y22 && y12 > y21


isOnSameSide :: Line -> Position -> Position -> Bool
isOnSameSide (start, end) point1 point2 =
  let line = positionToVector end `subtractVector` positionToVector start
      vector1 = positionToVector point1
      vector2 = positionToVector point2
  in (cosToSin $ angleCosBetweenVectors line vector1) `sameSign` (cosToSin $ angleCosBetweenVectors line vector2)
  where sameSign f1 f2 = (f1 >= 0 && f2 >= 0) || (f1 <= 0 && f2 <= 0)


-- | Checks if line collides with rectangle.
lineCollideWithTile :: Line -> Tile -> Bool
lineCollideWithTile line (Solid appearance) =
  let box = appearanceBox appearance
  in (collide line box) && (isLineInside line box)
  where
    isLineInside line' (Position (x1, y1), Position (x2, y2)) =
      (isOnSameSide line' Position (x1, y1))
      && (isOnSameSide line' Position (x1, y2))
      && (isOnSameSide line' Position (x2, y2))
      && (isOnSameSide line' Position (x2, y1))
lineCollideWithTile line _ = False


-- | Checks if object collide with map tile.
objectCollideWithTile :: Object -> Tile -> Bool
objectCollideWithTile _ (Transparent _) = False
objectCollideWithTile o (Solid app) =
  let boxes = objectCollisionBoxes o
      offset = objectPosition o
  in any (\box ->  collide (offsetRectangle offset box) (appearanceBox app)) boxes


-- | Returns all objects that collide with object.
objectsThatCollideWithObject :: Object -> Game -> [Object]
objectsThatCollideWithObject object game = filter (objectsCollide object) (levelObjects . gameLevel $ game)


-- | Returns all map tiles that collide with object.
tilesThatCollideWithObject :: Object -> Game -> [Tile]
tilesThatCollideWithObject object game = filter (objectCollideWithTile object) (concat . levelMap . gameLevel $ game)


rectanglesDistanceDown :: Rectangle -> Rectangle -> Float
rectanglesDistanceDown ((Position(x11, y11)), (Position(x12, _))) ((Position(x21, _)), (Position(x22, y22))) =
  if (x11 < x22 && x22 < x12) || (x11 < x21 && x21 < x12)
  then y11 - y22
  else (- infinity)


rectanglesDistanceUp :: Rectangle -> Rectangle -> Float
rectanglesDistanceUp r1 r2 = rectanglesCollideDown r2 r1


rectanglesDistanceLeft :: Rectangle -> Rectangle -> Float
rectanglesDistanceLeft ((Position(x11, y11)), (Position(_, y12))) ((Position(_, y21)), (Position(x22, y22))) =
  if (y11 < y22 && y22 < y12) || (y11 < y21 && y21 < y12)
  then x11 - x22
  else (- infinity)


rectanglesDistanceRight :: Rectangle -> Rectangle -> Float
rectanglesDistanceRight r1 r2 = rectanglesCollideLeft r2 r1


restrictionBoxForMovingObject :: Object -> Float -> Game -> Rectangle
restrictionBoxForMovingObject object time game =
  foldr (\tile rect ->
            foldr (shrinkRestriction $ tileRect tile) rect boxes
        ) infiniteRectangle tiles
  where
    boxes = objectCollisionBoxes object
    offset = objectPosition object
    velocity = objectVelocity object
    tiles = concat . levelMap . gameLevel $ game
    tileRect (Solid app) = appearanceBox app
    tileRect (Transparent _) = infiniteRectangle
    shrinkRestriction tileBox objBox restrBox =
      let vx = getX velocity
          vy = getY velocity
          dUp = rectanglesDistanceUp objBox tileBox
          dRight = rectanglesDistanceRight objBox tileBox
          dDown = rectanglesDistanceDown objBox tileBox
          dLeft = rectanglesDistanceLeft objBox tileBox
          x1 = fst . unwrap . fst $ objectBox
          y1 = snd . unwrap . fst $ objectBox
          x2 = fst . unwrap . snd $ objectBox
          y2 = snd . unwrap . snd $ objectBox
      in case defineDescartesFourth velocity of
           1 ->
             if (dUp > 0) && (dUp <= (vy + epsilon))
             then let dUp' = rectanglesDistanceUp (translateRectByX (vx * dUp / vy) objBox) tileBox
                  in if (dUp' > 0) && (dUp' <= (vy + epsilon))
                     then restrictUp restrBox tileBox
                     else restrBox
             else if (dRight > 0) && (dRight <= (vx + epsilon))
                  then let dRight' = rectanglesDistanceRight (translateRectByY (vy * dRight / vx) objBox) tileBox
                       in if (dRight' > 0) && (dRight' <= (vx + epsilon))
                          then restrictRight restrBox tileBox
                          else restrBox
                  else restrictTheSideLineCollideWith (Position (x2, y2), Position (x2 + vx, y2 + vy)) tileBox restrBox
           2 ->
             if (dUp > 0) && (dUp <= (vy + epsilon))
             then let dUp' = rectanglesDistanceUp (translateRectByX (vx * dUp / vy) objBox) tileBox
                  in if (dUp' > 0) && (dUp' <= (vy + epsilon))
                     then restrictUp restrBox tileBox
                     else restrBox
             else if (dLeft > 0) && (dLeft <= (- vx + epsilon))
                  then let dLeft' = rectanglesDistanceLeft (translateRectByY (vy * dLeft / (- vx)) objBox) tileBox
                       in if (dLeft' > 0) && (dLeft' <= (- vx + epsilon))
                          then restrictLeft restrBox tileBox
                          else restrBox
                  else restrictTheSideLineCollideWith (Position (x1, y2), Position (x1 + vx, y2 + vy)) tileBox restrBox
           3 ->
             if (dDown > 0) && (dDown <= (- vy + epsilon))
             then let dDown' = rectanglesDistanceDown (translateRectByX (vx * dDown / (- vy)) objBox) tileBox
                  in if (dDown' > 0) && (dDown' <= (- vy + epsilon))
                     then restrictDown restrBox tileBox
                     else restrBox
             else if (dLeft > 0) && (dLeft <= (- vx + epsilon))
                  then let dLeft' = rectanglesDistanceLeft (translateRectByY (vy * dLeft / (- vx)) objBox) tileBox
                       in if (dLeft' > 0) && (dLeft' <= (- vx + epsilon))
                          then restrictLeft restrBox tileBox
                          else restrBox
                  else restrictTheSideLineCollideWith (Position (x1, y1), Position (x1 + vx, y1 + vy)) tileBox restrBox
           otherwise ->
             if (dDown > 0) && (dDown <= (- vy + epsilon))
             then let dDown' = rectanglesDistanceDown (translateRectByX (vx * dDown / (- vy)) objBox) tileBox
                  in if (dDown' > 0) && (dDown' <= (- vy + epsilon))
                     then restrictDown restrBox tileBox
                     else restrBox
             else if (dRight > 0) && (dRight <= (vx + epsilon))
                  then let dRight' = rectanglesDistanceRight (translateRectByY (vy * dRight / vx) objBox) tileBox
                       in if (dRight' > 0) && (dRight' <= (vx + epsilon))
                          then restrictRight restrBox tileBox
                          else restrBox
                  else restrictTheSideLineCollideWith (Position (x2, y1), Position (x2 + vx, y1 + vy)) tileBox restrBox
    restrictDown (Position (rx1, ry1), Position (rx2, ry2)) (_, Position (_, ty)) =
      if ty > ry1
      then (Position (rx1, ty), Position (rx2, ry2))
      else (Position (rx1, ry1), Position (rx2, ry2))
    restrictUp (Position (rx1, ry1), Position (rx2, ry2)) (Position (_, ty), _) =
      if ty < ry2
      then (Position (rx1, ry1), Position (rx2, ty))
      else (Position (rx1, ry1), Position (rx2, ry2))
    restrictLeft (Position (rx1, ry1), Position (rx2, ry2)) (Position (tx, _), _) =
      if tx > rx1
      then (Position (tx, ry1), Position (rx2, ry2))
      else (Position (rx1, ry1), Position (rx2, ry2))
    restrictRight (Position (rx1, ry1), Position (rx2, ry2)) (_, Position (rx, _)) =
      if tx < rx2
      then (Position (rx1, ry1), Position (tx, ry2))
      else (Position (rx1, ry1), Position (rx2, ry2))
    restrictTheSideLineCollideWith line tileBox restrBox =


-- tilesThatCollideWithMovingObject :: Object -> Game -> [Tile]
-- tilesThatCollideWithMovingObject object game =
--   let boxes = objectCollisionBoxes object
--       offset = objectPosition object
--       velocity = objectVelocity object
--       tiles = concat . levelMap . gameLevel $ game
--   in filter (\tile ->
--                 any (\box ->
--                         movingObjectCollideWithTile (offsetRectangle offset box) velocity tile
--                     ) boxes
--             ) tiles
--   where
--     movingObjectCollideWithTile (Position (x1, y1), Position (x2, y2)) (Vector (x, y)) tile =
--       lineCollideWithTile (Position (x1, y1), Position (x1 + x, y1 + y)) tile
--       || lineCollideWithTile (Position (x1, y2), Position (x1 + x, y2 + y)) tile
--       || lineCollideWithTile (Position (x2, y2), Position (x2 + x, y2 + y)) tile
--       || lineCollideWithTile (Position (x2, y1), Position (x2 + x, y1 + y)) tile
--

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
updatePhysics time game =
    game { gamePlayers = map updatePlayer (gamePlayers game)
         , gameLevel = (gameLevel game) {
             levelObjects = map updateObject (levelObjects . gameLevel $ game)
           }
         }
  where
    updatePlayer = movePlayer . updatePlayerVelocity time . acceleratePlayer
    updateObject = moveObject time game . updateObjectVelocity time . accelerateObject
    acceleratePlayer player = player { playerObject = accelerateObject (playerObject player) }
    accelerateObject = updateAcceleration game . nullifyAcceleration
    movePlayer player = player { playerObject = moveObject time game (playerObject player) }


moveObject :: Time -> Game -> Object -> Object
moveObject time game obj =
  let resObj = {- restrictObjectRelToObjects game -} (restrictObjectRelToTiles game obj)
  in resObj { objectPosition = performMove time (objectVelocity resObj) (objectPosition resObj)
            }


restrictObjectRelToTiles :: Float -> Game -> Object -> Object
restrictObjectRelToTiles time game obj =
  let vel = objectVelocity obj
      pos = objectPosition obj
      newObj = obj { objectPosition = performMove time vel pos }
      oldColTiles = tilesThatCollideWithObject obj game
      newColTiles = tilesThatCollideWithMovingObject newObj game
      diffColTiles = newColTiles \\ oldColTiles
  in if null diffColTiles
     then obj
     else restrictObject object (restrictionBox object diffColTiles)
  where restrictObject object box =


restrictionBox :: Object -> [Tile] -> Rectangle
restrictionBox object tiles = foldr shrinkBox infiniteRectangle tiles
  where shrinkBox tile box =


performMove :: Time -> Vector -> Position -> Position
performMove time (Vector (vx, vy)) (Position (x, y)) =
  Position ( x + vx * time * standardVelocity
           , y + vy * time * standardVelocity
           )


objectOnGround :: Object -> Map -> Bool
objectOnGround object tiles = any (objectCollideWithTile object) (concat tiles)


frictionVector :: Object -> Vector
frictionVector obj = takeShortest newVector (invertVector . objectVelocity $ obj)
  where velDir = normalizeVector . objectVelocity $ obj
        newVector = invertVector velDir `mulByNumber` frictionCoef


reactVector :: Object -> Vector
reactVector obj = frictionVector obj `plus` invertVector gravitationalVector


updatePlayerVelocity :: Float -> Player -> Player
updatePlayerVelocity time player =
  player { playerObject = addControlVector (updateObjectVelocity time . playerObject $ player) (playerControlVector player)
         }


updateObjectVelocity :: Float -> Object -> Object
updateObjectVelocity time object =
  object { objectVelocity = positionToVector $ newVelocity (objectAcceleration object) (vectorToPosition . objectVelocity $ object)
         }
  where
    newVelocity acc vel = performMove time acc vel


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


updateAcceleration :: Game -> Object -> Object
updateAcceleration game object = addReactElement . addGravitationalElement $ object
  where
    addGravitationalElement obj =
      obj { objectAcceleration = objectAcceleration obj `plus` gravitationalVector
          }
    addReactElement obj =
      if objectOnGround obj (levelMap . gameLevel $ game)
      then obj { objectAcceleration = objectAcceleration obj `plus` reactVector obj
               }
      else obj


nullifyAcceleration :: Object -> Object
nullifyAcceleration object =
  object { objectAcceleration = zeroVector
         }



movePlayer :: Vector -> Action
movePlayer vector =
  PlayerAction (\player _ ->
    let currentControlVector = playerControlVector player
    in player { playerControlVector = vector `plus` currentControlVector }
  )


jumpPlayer :: Vector -> Action
jumpPlayer vector =
  PlayerAction (\player game ->
    let
      obj = playerObject player
      currentVelocity = objectVelocity obj
    in if objectOnGround (playerObject player) (levelMap . gameLevel $ game)
       then player
              { playerObject = obj { objectVelocity = currentVelocity `plus` vector
                                   }
              }
      else player
  )


zeroAction :: Action
zeroAction = PlayerAction (\p _ -> p)


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