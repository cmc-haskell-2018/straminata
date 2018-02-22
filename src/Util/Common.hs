-- | Contains commonly used functions.
module Util.Common where

import Model.CommonTypes
import Visual.WindowConstants

frameWidth :: Float
frameWidth = 25

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
    displayBoxes = map (offsetDisplayBox . objectHitbox . playerObject) (gamePlayers game)
    offsetDisplayBox = \hitbox -> offsetRectangle (hitboxPosition hitbox) (hitboxDisplayBox hitbox)
    position = (Vector (rect !! 0, rect !! 1) `plus` Vector (rect !! 2, rect !! 3)) `divByNumber` 2
    rect = [ minimum (map (fst . unwrap . fst) displayBoxes) - frameWidth
           , minimum (map (snd . unwrap . fst) displayBoxes) - frameWidth
           , maximum (map (fst . unwrap . snd) displayBoxes) + frameWidth
           , maximum (map (snd . unwrap . snd) displayBoxes) + frameWidth
           ]
    boundaryDimensions = (rect !! 2 - rect !! 0, rect !! 3 - rect !! 1)
    -- todo: indentWindowContent is quick workaround; add better way to create additional bounds to window contents.
    ratio = minimum [ (fromIntegral . fst $ initialWindowDimensions) / (fst boundaryDimensions)
                    , (fromIntegral . snd $ initialWindowDimensions) / (snd boundaryDimensions)
                    , 1
                    ]