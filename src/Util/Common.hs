-- | Contains commonly used functions.
module Util.Common where

import Data.List(isInfixOf)

import Model.CommonTypes
import Visual.WindowConstants
import Util.Constants

frameWidth :: Float
frameWidth = 5 * level1TileSize


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


translateRectByVector :: Vector -> Rectangle -> Rectangle
translateRectByVector (Vector (x, y)) (Position (x1, y1), Position (x2, y2)) =
  (Position (x1 + x, y1 + y), Position (x2 + x, y2 + y))

-- | Checks if two rectangles are colliding.
collide :: Rectangle -> Rectangle -> Bool
collide (Position (x11, y11), Position (x12, y12))
        (Position (x21, y21), Position (x22, y22)) = x11 < x22 && x12 > x21 && y11 < y22 && y12 > y21


-- | Checks if line collides with another line.
linesCollide :: Line -> Line -> Bool
linesCollide (Position (x11, y11), Position (x12, y12)) (Position (x21, y21), Position (x22, y22)) =
  let a = (x22 - x21) * (y11 - y21) - (y22 - y21) * (x11 - x21) > 0
      b = (x22 - x21) * (y12 - y21) - (y22 - y21) * (x12 - x21) > 0
      c = (x12 - x11) * (y21 - y11) - (y12 - y11) * (x21 - x11) > 0
      d = (x12 - x11) * (y22 - y11) - (y12 - y11) * (x22 - x11) > 0
  in a /= b && c /= d


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


tileRect :: Tile -> Rectangle
tileRect (Solid app) = appearanceBox app
tileRect (Transparent _) = infiniteRectangle


rectanglesDirectlyDownDistance :: Rectangle -> Rectangle -> Float
rectanglesDirectlyDownDistance ((Position(x11, y11)), (Position(x12, _))) ((Position(x21, _)), (Position(x22, y22))) =
  if (x11 <= x22 && x22 <= x12) || (x11 <= x21 && x21 <= x12) || (x21 <= x11 && x11 <= x22)
  then y11 - y22
  else (- infinity)


rectanglesDirectlyUpDistance :: Rectangle -> Rectangle -> Float
rectanglesDirectlyUpDistance r1 r2 = rectanglesDirectlyDownDistance r2 r1


rectanglesDirectlyLeftDistance :: Rectangle -> Rectangle -> Float
rectanglesDirectlyLeftDistance ((Position(x11, y11)), (Position(_, y12))) ((Position(_, y21)), (Position(x22, y22))) =
  if (y11 <= y22 && y22 <= y12) || (y11 <= y21 && y21 <= y12) || (y21 <= y11 && y11 <= y22)
  then x11 - x22
  else (- infinity)


rectanglesDirectlyRightDistance :: Rectangle -> Rectangle -> Float
rectanglesDirectlyRightDistance r1 r2 = rectanglesDirectlyLeftDistance r2 r1


rectanglesDistanceDown :: Rectangle -> Rectangle -> Float
rectanglesDistanceDown ((Position(_, y11)), _) (_, (Position(_, y22))) = y11 - y22


rectanglesDistanceUp :: Rectangle -> Rectangle -> Float
rectanglesDistanceUp r1 r2 = rectanglesDistanceDown r2 r1


rectanglesDistanceLeft :: Rectangle -> Rectangle -> Float
rectanglesDistanceLeft ((Position(x11, _)), _) (_, (Position(x22, _))) = x11 - x22


rectanglesDistanceRight :: Rectangle -> Rectangle -> Float
rectanglesDistanceRight r1 r2 = rectanglesDistanceLeft r2 r1


restrictMovingObject :: Object -> Float -> Game -> Object
restrictMovingObject object time game = object {objectVelocity = restrictedVelocity}
  where
    boxes = map (offsetRectangle offset) (objectCollisionBoxes object)
    offset = objectPosition object
    velocity = objectVelocity object
    tiles = concat . levelMap . gameLevel $ game
    objects = filter isNotActive . levelObjects . gameLevel $ game
    restrictedVelocity = foldr
      (\rect vel -> foldr (restrictVelocity rect) vel boxes)
      velocity
      ((map tileRect . filter isSolid $ tiles) ++ (concat (map (\o -> map (offsetRectangle (objectPosition o)) $ objectCollisionBoxes o) objects)))
    restrictVelocity tileBox objBox newVelocity =
      let newObjBox = translateRectByVector (newVelocity `mulByNumber` time) objBox
          dUp = rectanglesDistanceUp objBox tileBox
          dDown = rectanglesDistanceDown objBox tileBox
          dLeft = rectanglesDistanceLeft objBox tileBox
          dRight = rectanglesDistanceRight objBox tileBox
          maybeRestrictUp velocity' = if dUp >= 0 then restrictUp velocity' dUp else velocity'
          maybeRestrictDown velocity' = if dDown >= 0 then restrictDown velocity' dDown else velocity'
          maybeRestrictLeft velocity' = if dLeft >= 0 then restrictLeft velocity' dLeft else velocity'
          maybeRestrictRight velocity' = if dRight >= 0 then restrictRight velocity' dRight else velocity'
          restrictUp (Vector (x, _)) dist = (Vector (x, (dist / time) - epsilon))
          restrictDown (Vector (x, _)) dist = (Vector (x, - (dist / time) + epsilon))
          restrictLeft (Vector (_, y)) dist = (Vector (- (dist / time) + epsilon, y))
          restrictRight (Vector (_, y)) dist = (Vector ((dist / time) - epsilon, y))
      in if collide tileBox objBox
         then newVelocity
         else if collide tileBox newObjBox
              then maybeRestrictUp . maybeRestrictDown . maybeRestrictLeft . maybeRestrictRight $ newVelocity
              else newVelocity


isNotActive :: Object -> Bool
isNotActive obj = let name = objectName obj
               in not $ isInfixOf "button" name || isInfixOf "finish" name || isInfixOf "coin" name


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
                    , 6 - level1TileSize / 10
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
    acceleratePlayer player =
      player { playerObject =
        updateAcceleration time game . addControlVector time (playerControlVector player) . nullifyAcceleration $ (playerObject player)
      }
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


objectOnGround :: Object -> Map -> [Object] -> Bool
objectOnGround object tiles objects =
  any (\tile -> any (onGround tile) boxes ) ((map tileRect $ concat tiles)
       ++ (concat (map (\o -> map (offsetRectangle (objectPosition o)) $ objectCollisionBoxes o) objects)))
  where
    boxes = map (offsetRectangle offset) (objectCollisionBoxes object)
    offset = objectPosition object
    onGround tile box =
      let dist = rectanglesDirectlyDownDistance box tile
      in (dist < epsilon) && (dist > (- epsilon))


frictionVector :: Float -> Object -> Vector
frictionVector time obj = takeShortest newVector (flatVelocity `divByNumber` time)
  where flatVelocity = invertVector (projectVector (objectVelocity obj) (Vector (1, 0)))
        frictionDir = normalizeVector flatVelocity
        newVector = frictionDir `mulByNumber` (frictionCoef)


reactVector :: Float -> Game -> Object -> Vector
reactVector time game obj =
  let tiles = filter isSolid (concat . levelMap . gameLevel $ game)
      boxes = map (offsetRectangle offset) (objectCollisionBoxes obj)
      offset = objectPosition obj
      accel = objectAcceleration obj
      vel = objectVelocity obj `divByNumber` time
  in frictionVector time obj
     `plus` if any (\tile -> any (touchDown (tileRect tile)) boxes) tiles
            then (projectVector' accel (Vector (0, 1)) `plus` projectVector' vel (Vector (0, 1)))
            else zeroVector
     `plus` if any (\tile -> any (touchUp (tileRect tile)) boxes) tiles
            then (projectVector' accel (Vector (0, -1)) `plus` projectVector' vel (Vector (0, -1)))
            else zeroVector
     `plus` if any (\tile -> any (touchLeft (tileRect tile)) boxes) tiles
            then (projectVector' accel (Vector (1, 0)) `plus` projectVector' vel (Vector (1, 0)))
            else zeroVector
     `plus` if any (\tile -> any (touchRight (tileRect tile)) boxes) tiles
            then (projectVector' accel (Vector (-1, 0)) `plus` projectVector' vel (Vector (-1, 0)))
            else zeroVector
  where
    projectVector' v1 v2 = normalizeVector v2 `mulByNumber` vectorLength (projectVector v1 v2)
    touchUp tile box =
      let dist = rectanglesDirectlyUpDistance box tile
      in (dist < 2 * epsilon) && (dist > (-2 * epsilon))
    touchDown tile box =
      let dist = rectanglesDirectlyDownDistance box tile
      in  (dist < 2 * epsilon) && (dist > (-2 * epsilon))
    touchLeft tile box =
      let dist = rectanglesDirectlyLeftDistance box tile
      in (dist < 2 * epsilon) && (dist > (-2 * epsilon))
    touchRight tile box =
      let dist = rectanglesDirectlyRightDistance box tile
      in (dist < 2 * epsilon) && (dist > (-2 * epsilon))


updatePlayerVelocity :: Float -> Player -> Player
updatePlayerVelocity time player =
  player { playerObject = updateObjectVelocity time . playerObject $ player }


updateObjectVelocity :: Float -> Object -> Object
updateObjectVelocity time object =
  object { objectVelocity = newVelocity (objectAcceleration object) (vectorToPosition . objectVelocity $ object) }
  where
    newVelocity acc vel = restrictByMaxVelocity . positionToVector $ performMove time acc vel


restrictByMaxVelocity :: Vector -> Vector
restrictByMaxVelocity (Vector (x, y))
  | abs x > maxVelocity && abs y > maxVelocity = (Vector ((signum x) * maxVelocity, (signum y) * maxVelocity))
  | abs x > maxVelocity = (Vector ((signum x) * maxVelocity, y))
  | abs y > maxVelocity = (Vector (x, (signum y) * maxVelocity))
  | otherwise = (Vector (x, y))


addControlVector :: Float -> Vector -> Object -> Object
addControlVector time con obj =
  let control = con `divByNumber` time
      accel = objectAcceleration obj
      proj = projectVector accel control
      angle = angleCosBetweenVectors control proj
      newAcceleration = if angle < 0
                    then accel `plus` control
                    else if (vectorLength proj) > (vectorLength control)
                         then accel
                         else accel `plus` (control `subtractVector` proj)
  in obj { objectAcceleration = newAcceleration }


updateAcceleration :: Float -> Game -> Object -> Object
updateAcceleration time game object =
  if objectAffectedByGravity object
  then addReactElement . addGravitationalElement $ object
  else object
  where
    addGravitationalElement obj =
      obj { objectAcceleration = objectAcceleration obj `plus` gravitationalVector }
    addReactElement obj =
      obj { objectAcceleration = objectAcceleration obj `plus` reactVector time game obj }


nullifyAcceleration :: Object -> Object
nullifyAcceleration object =
  object { objectAcceleration = zeroVector }


movePlayer :: Vector -> Action
movePlayer vector =
  PlayerAction (\player _ ->
    let currentControlVector = playerControlVector player
    in player { playerControlVector = vector `plus` currentControlVector }
  )


jumpPlayer :: Vector -> Animation -> Action
jumpPlayer vector jumpAnimation =
  PlayerAction (\player game ->
    let
      obj = playerObject player
      currentVelocity = objectVelocity obj
      changeAnimation player' = player' { playerObject = (playerObject player')
                                                           { objectAppearance = (objectAppearance . playerObject $ player') {appearanceAnimation = jumpAnimation} } }
    in changeAnimation (if objectOnGround (playerObject player) (levelMap . gameLevel $ game) (levelObjects . gameLevel $ game)
       then player
              { playerObject = obj { objectVelocity = currentVelocity `plus` vector
                                   }
              }
      else player)
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


activateObject :: Bool -> Action
activateObject state =
  GameAction (\player game ->
    foldr (\obj acc -> (objectOnActivate obj) state player obj acc) game (filter (\obj -> objectsCollide (playerObject player) obj) (objects game))
  )
  where objects game = levelObjects . gameLevel $ game


setTextureByName :: String -> Texture -> Action
setTextureByName name texture =
  GameAction (\_ game ->
    game { gamePlayers =
             map (\player -> if isTarget player
                             then player {
                                    playerObject = (playerObject player) {
                                      objectAppearance = (objectAppearance . playerObject $ player) {
                                        appearanceActualSize = fst texture
                                      , appearanceAnimation = [snd texture]
                                      }
                                    }
                                  }
                             else player
             ) (gamePlayers game)
         }
  )
  where isTarget player = (objectName . playerObject $ player) == name


updateAnimations :: Game -> Game
updateAnimations game =
  game { gamePlayers = map updatePlayer (gamePlayers game)
       , gameLevel = (gameLevel game) {
           levelObjects = map updateObject (levelObjects . gameLevel $ game)
         }
       }
  where
    updatePlayer player =
      player { playerObject = updateObject (playerObject player) }
    updateObject object =
      object { objectAppearance = (objectAppearance object) { appearanceAnimation = cutHead . appearanceAnimation . objectAppearance $ object } }
    cutHead [ x ] = [ x ]
    cutHead (x : xs) = xs


changeTexture :: Texture -> Object -> Object
changeTexture texture object = object
  { objectAppearance = (objectAppearance object)
      { appearanceActualSize = fst texture
      , appearanceAnimation = [snd texture]
      }
  }