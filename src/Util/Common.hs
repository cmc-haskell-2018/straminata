-- | Contains commonly used functions.
module Util.Common where

import Model.CommonTypes

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
                (Position (x1, y1), Position (x2, y2)) = (Position (x1 / 2 + x, y1 / 2 + y), Position (x2 / 2 + x, y2 / 2 + y))

-- | Checks if two rectangles are colliding.
collide :: Rectangle -> Rectangle -> Bool
collide (Position (x11, y11), Position (x12, y12))
          (Position (x21, y21), Position (x22, y22)) =
            x11 < x22 && x12 > x21 && y11 < y22 && y12 > y21