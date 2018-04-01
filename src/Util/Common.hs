-- | Contains commonly used functions.
module Util.Common where

import Model.CommonTypes
import Visual.WindowConstants
import Util.Constants

frameWidth :: Float
frameWidth = 100

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
        (Position (x21, y21), Position (x22, y22)) = x11 <= x22 && x12 >= x21 && y11 <= y22 && y12 >= y21


isOnSameSide :: Line -> (Position, Position) -> Bool
isOnSameSide (start, end) (point1, point2) =
  let line = positionToVector end `subtractVector` positionToVector start
      vector1 = positionToVector point1
      vector2 = positionToVector point2
  in (cosToSin $ angleCosBetweenVectors line vector1) `sameSign` (cosToSin $ angleCosBetweenVectors line vector2)
  where sameSign f1 f2 = (f1 >= 0 && f2 >= 0) || (f1 <= 0 && f2 <= 0)


-- | Checks if line collides with another line.
linesCollide :: Line -> Line -> Bool
linesCollide line1 line2 = (collide line1 line2) && (not $ isOnSameSide line1 line2)


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


rectanglesDistanceDown :: Rectangle -> Rectangle -> Float
rectanglesDistanceDown ((Position(x11, y11)), (Position(x12, _))) ((Position(x21, _)), (Position(x22, y22))) =
  if (x11 <= x22 && x22 <= x12) || (x11 <= x21 && x21 <= x12) || (x21 <= x11 && x11 <= x22)
  then y11 - y22
  else (- infinity)


rectanglesDistanceUp :: Rectangle -> Rectangle -> Float
rectanglesDistanceUp r1 r2 = rectanglesDistanceDown r2 r1


rectanglesDistanceLeft :: Rectangle -> Rectangle -> Float
rectanglesDistanceLeft ((Position(x11, y11)), (Position(_, y12))) ((Position(_, y21)), (Position(x22, y22))) =
  if (y11 <= y22 && y22 <= y12) || (y11 <= y21 && y21 <= y12) || (y21 <= y11 && y11 <= y22)
  then x11 - x22
  else (- infinity)


rectanglesDistanceRight :: Rectangle -> Rectangle -> Float
rectanglesDistanceRight r1 r2 = rectanglesDistanceLeft r2 r1


restrictMovingObject :: Object -> Float -> Game -> Object
restrictMovingObject object time game = object {objectVelocity = restrictedVelocity}
 where
    restrictedVelocity =
      foldr (\tile vel -> if isSolid tile
                          then foldr (restrictVelocity $ tileRect tile) vel boxes
                          else vel
            ) velocity tiles
    boxes = map (offsetRectangle offset) (objectCollisionBoxes object)
    offset = objectPosition object
    velocity = objectVelocity object
    tiles = concat . levelMap . gameLevel $ game
    tileRect (Solid app) = appearanceBox app
    tileRect (Transparent _) = infiniteRectangle
    restrictVelocity tileBox objBox newVelocity =
      let vx = getX newVelocity * time
          vy = getY newVelocity * time
          dUp = rectanglesDistanceUp objBox tileBox
          dRight = rectanglesDistanceRight objBox tileBox
          dDown = rectanglesDistanceDown objBox tileBox
          dLeft = rectanglesDistanceLeft objBox tileBox
          x1 = fst . unwrap . fst $ objBox
          y1 = snd . unwrap . fst $ objBox
          x2 = fst . unwrap . snd $ objBox
          y2 = snd . unwrap . snd $ objBox
      in case defineDescartesQuadrant newVelocity of
           1 ->
             if (dUp >= 0) && (dUp <= (vy + epsilon))
             then let dUp' = rectanglesDistanceUp (translateRectByX (vx * dUp / vy) objBox) tileBox
                  in if (dUp' >= 0) && (dUp' <= (vy + epsilon))
                     then restrictUp newVelocity dUp
                     else newVelocity
             else if (dRight >= 0) && (dRight <= (vx + epsilon))
                  then let dRight' = rectanglesDistanceRight (translateRectByY (vy * dRight / vx) objBox) tileBox
                       in if (dRight' >= 0) && (dRight' <= (vx + epsilon))
                          then restrictRight newVelocity dRight
                          else newVelocity
                  else restrictRightOrUp (Position (x2, y2), Position (x2 + vx, y2 + vy)) newVelocity tileBox
           2 ->
             if (dUp >= 0) && (dUp <= (vy + epsilon))
             then let dUp' = rectanglesDistanceUp (translateRectByX (vx * dUp / vy) objBox) tileBox
                  in if (dUp' >= 0) && (dUp' <= (vy + epsilon))
                     then restrictUp newVelocity dUp
                     else newVelocity
             else if (dLeft >= 0) && (dLeft <= (- vx + epsilon))
                  then let dLeft' = rectanglesDistanceLeft (translateRectByY (vy * dLeft / (- vx)) objBox) tileBox
                       in if (dLeft' >= 0) && (dLeft' <= (- vx + epsilon))
                          then restrictLeft newVelocity dLeft
                          else newVelocity
                  else restrictLeftOrUp (Position (x1, y2), Position (x1 + vx, y2 + vy)) newVelocity tileBox
           3 ->
             if (dDown >= 0) && (dDown <= (- vy + epsilon))
             then let dDown' = rectanglesDistanceDown (translateRectByX (vx * dDown / (- vy)) objBox) tileBox
                  in if (dDown' >= 0) && (dDown' <= (- vy + epsilon))
                     then restrictDown newVelocity dDown
                     else newVelocity
             else if (dLeft >= 0) && (dLeft <= (- vx + epsilon))
                  then let dLeft' = rectanglesDistanceLeft (translateRectByY (vy * dLeft / (- vx)) objBox) tileBox
                       in if (dLeft' >= 0) && (dLeft' <= (- vx + epsilon))
                          then restrictLeft newVelocity dLeft
                          else newVelocity
                  else restrictLeftOrDown (Position (x1, y1), Position (x1 + vx, y1 + vy)) newVelocity tileBox
           _ ->
             if (dDown >= 0) && (dDown <= (- vy + epsilon))
             then let dDown' = rectanglesDistanceDown (translateRectByX (vx * dDown / (- vy)) objBox) tileBox
                  in if (dDown' >= 0) && (dDown' <= (- vy + epsilon))
                     then restrictDown newVelocity dDown
                     else newVelocity
             else if (dRight >= 0) && (dRight <= (vx + epsilon))
                  then let dRight' = rectanglesDistanceRight (translateRectByY (vy * dRight / vx) objBox) tileBox
                       in if (dRight' >= 0) && (dRight' <= (vx + epsilon))
                          then restrictRight newVelocity dRight
                          else newVelocity
                  else restrictRightOrDown (Position (x2, y1), Position (x2 + vx, y1 + vy)) newVelocity tileBox
    restrictDown (Vector (x, _)) dist = (Vector (x, - (dist / time) + epsilon))
    restrictUp (Vector (x, _)) dist = (Vector (x, (dist / time) - epsilon))
    restrictLeft (Vector (_, y)) dist = (Vector (- (dist / time) + epsilon, y))
    restrictRight (Vector (_, y)) dist = (Vector ((dist / time) - epsilon, y))
    restrictRightOrUp (Position (lx1, ly1), Position (lx2, ly2)) vel (Position (x1, y1), Position (x2, y2)) =
      if linesCollide (Position (lx1, ly1), Position (lx2, ly2)) (Position (x1, y1), Position (x1, y2))
      then restrictRight vel (x1 - lx1)
      else if linesCollide (Position (lx1, ly1), Position (lx2, ly2)) (Position (x1, y1), Position (x2, y1))
           then restrictUp vel (y1 - ly1)
           else vel
    restrictLeftOrUp (Position (lx1, ly1), Position (lx2, ly2)) vel (Position (x1, y1), Position (x2, y2)) =
      if linesCollide (Position (lx1, ly1), Position (lx2, ly2)) (Position (x2, y1), Position (x2, y2))
      then restrictLeft vel (lx1 - x2)
      else if linesCollide (Position (lx1, ly1), Position (lx2, ly2)) (Position (x1, y1), Position (x2, y1))
           then restrictUp vel (y1 - ly1)
           else vel
    restrictLeftOrDown (Position (lx1, ly1), Position (lx2, ly2)) vel (Position (x1, y1), Position (x2, y2)) =
      if linesCollide (Position (lx1, ly1), Position (lx2, ly2)) (Position (x2, y1), Position (x2, y2))
      then restrictLeft vel (lx1 - x2)
      else if linesCollide (Position (lx1, ly1), Position (lx2, ly2)) (Position (x1, y2), Position (x2, y2))
           then restrictDown vel (ly1 - y2)
           else vel
    restrictRightOrDown (Position (lx1, ly1), Position (lx2, ly2)) vel (Position (x1, y1), Position (x2, y2)) =
      if linesCollide (Position (lx1, ly1), Position (lx2, ly2)) (Position (x1, y1), Position (x1, y2))
      then restrictRight vel (x1 - lx1)
      else if linesCollide (Position (lx1, ly1), Position (lx2, ly2)) (Position (x1, y2), Position (x2, y2))
           then restrictDown vel (ly1 - y2)
           else vel


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
    updatePlayer = updatePlayerPosition . updatePlayerVelocity time . acceleratePlayer
    updateObject = moveObject time game . updateObjectVelocity time . accelerateObject
    acceleratePlayer player = player { playerObject = accelerateObject (playerObject player) }
    accelerateObject = updateAcceleration time game . nullifyAcceleration
    updatePlayerPosition player = player { playerObject = moveObject time game (playerObject player) }


moveObject :: Time -> Game -> Object -> Object
moveObject time game obj =
  let resObj = {- restrictObjectRelToObjects game -} (restrictMovingObject obj time game)
  in resObj { objectPosition = performMove time (objectVelocity resObj) (objectPosition resObj)
            }


performMove :: Time -> Vector -> Position -> Position
performMove time (Vector (vx, vy)) (Position (x, y)) =
  Position ( x + vx * time
           , y + vy * time
           )


objectOnGround :: Object -> Map -> Bool
objectOnGround object tiles =
  any (\tile -> any (onGround tile) boxes ) (map tileRect $ concat tiles)
  where
    boxes = map (offsetRectangle offset) (objectCollisionBoxes object)
    offset = objectPosition object
    tileRect (Solid app) = appearanceBox app
    tileRect (Transparent _) = infiniteRectangle
    onGround tile box =
      let dist = rectanglesDistanceDown box tile
      in (dist < epsilon) && (dist > (- epsilon))


frictionVector :: Float -> Object -> Vector
frictionVector time obj = takeShortest newVector ((invertVector . objectVelocity $ obj) `divByNumber` time)
  where velDir = normalizeVector . objectVelocity $ obj
        newVector = invertVector velDir `mulByNumber` (frictionCoef * g)


reactVector :: Float -> Object -> Vector
reactVector time obj = frictionVector time obj `plus` invertVector gravitationalVector


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


updateAcceleration :: Float -> Game -> Object -> Object
updateAcceleration time game object =
  if objectAffectedByGravity object
  then addReactElement . addGravitationalElement $ object
  else object
  where
    addGravitationalElement obj =
      obj { objectAcceleration = objectAcceleration obj `plus` gravitationalVector
          }
    addReactElement obj =
      if objectOnGround obj (levelMap . gameLevel $ game)
      then obj { objectAcceleration = objectAcceleration obj `plus` reactVector time obj
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


flightPlayer :: Vector -> Action
flightPlayer vector =
  PlayerAction (\player _ ->
    let obj = playerObject player
        vel = objectVelocity obj
        projVel = projectVector vel vector
    in player { playerObject =
         obj {
           objectVelocity =
             if vectorLength vector < vectorLength projVel
             then vel
             else vel `subtractVector` projVel `plus` vector
         }
       }
  )


stopFlightPlayer :: Vector -> Action
stopFlightPlayer vector =
  PlayerAction (\player _ ->
    let obj = playerObject player
        vel = objectVelocity obj
        projVel = projectVector vel vector
    in player { playerObject =
         obj {
           objectVelocity =
             if vectorLength vector > vectorLength projVel
             then vel
             else vel `subtractVector` vector
         }
       }
  )


zeroAction :: Action
zeroAction = PlayerAction (\p _ -> p)


switchControlsAction :: PlayerControls -> Action
switchControlsAction controls = PlayerAction (\player _ -> player { playerControls = controls })


setAffectionByGravity :: Bool -> Action
setAffectionByGravity val =
  PlayerAction (\player _ ->
    player { playerObject =
      (playerObject player) { objectAffectedByGravity = val }
    }
  )


stopPlayer :: Action
stopPlayer =
 PlayerAction (\player _ ->
   let
     obj = playerObject player
   in player
        { playerObject = obj { objectVelocity = zeroVector
                             }
        }
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


changeTexture :: Texture -> Object -> Object
changeTexture texture object = object
  { objectAppearance = (objectAppearance object)
      { appearanceActualSize = fst texture
      , appearancePicture = snd texture
      }
  }