-- | Contains commonly used functions
module Util.Common where

import Model.CommonTypes

hitboxesCollide :: Hitbox -> Hitbox -> Bool
hitboxesCollide h1 h2 = let boxes1 = collisionBoxes h1
                            boxes2 = collisionBoxes h2
                            offset1 = position h1
                            offset2 = position h2
                        in any (\box1 -> any (\box2 -> collide (offsetRectangle offset1 box1)
                                                               (offsetRectangle offset2 box2)
                                             ) boxes2
                               ) boxes1

offsetRectangle :: Position -> Rectangle -> Rectangle
offsetRectangle (Position (x, y))
                (Rectangle ( (Position (x1, y1))
                           , (Position (x2, y2)))) = Rectangle (Position (x1 / 2 + x, y1 / 2 + y), Position (x2 / 2 + x, y2 / 2 + y))

collide :: Rectangle -> Rectangle -> Bool
collide (Rectangle ( (Position (x11, y11))
                   , (Position (x12, y12))))
        (Rectangle ( (Position (x21, y21))
                   , (Position (x22, y22)))) =
            x11 < x22 && x12 > x21 && y11 < y22 && y12 > y21