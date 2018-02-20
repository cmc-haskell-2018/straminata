-- | Contains commonly used functions.
module Util.Common where

import Model.CommonTypes
import Visual.WindowConstants
-- | Check if two hitboxes collide.
hitboxesCollide :: Hitbox -> Hitbox -> Bool
hitboxesCollide h1 h2 = let boxes1 = hitboxCollisionBoxes h1
                            boxes2 = hitboxCollisionBoxes h2
                            offset1 = hitboxPosition h1
                            offset2 = hitboxPosition h2
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
    playerHitboxes = map (objectHitbox . playerObject) (gamePlayers game)
    displayBoxes = map offsetDisplayBox playerHitboxes
    centers = map boxCenter displayBoxes
    offsetDisplayBox = \hitbox -> offsetRectangle (hitboxPosition hitbox) (hitboxDisplayBox hitbox)
    boxCenter = \box -> ((positionToVector . fst $ box) `plus` (positionToVector . snd $ box)) `divByNumber` 2.0
    position = foldr plus (Vector (0, 0)) centers `divByNumber` (fromIntegral . length $ centers)
    rect = [ minimum (map (fst . unwrap . fst) displayBoxes)
           , minimum (map (snd . unwrap . fst) displayBoxes)
           , maximum (map (fst . unwrap . snd) displayBoxes)
           , maximum (map (snd . unwrap . snd) displayBoxes)
           ]
    boundaryDimensions = (rect !! 2 - rect !! 0, rect !! 3 - rect !! 1)
    -- todo: indentWindowContent is quick workaround; add better way to create additional bounds to window contents.
    ratio = minimum [ (fromIntegral . indentWindowContent . fst $ initialWindowDimensions) / (fst boundaryDimensions)
                    , (fromIntegral . indentWindowContent . snd $ initialWindowDimensions) / (snd boundaryDimensions)
                    , 1
                    ]