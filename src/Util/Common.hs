-- | Contains commonly used functions
module Util.Common where

import Model.CommonTypes

-- todo 17.02.18: generalize for any type of polygon
formPath :: Position -> Dimensions -> [(Float, Float)]
formPath (Position (x, y))
         (Dimensions (halfWidth, halfHeight)) = [ (x - halfWidth, y + halfHeight)
                                                , (x + halfWidth, y + halfHeight)
                                                , (x + halfWidth, y - halfHeight)
                                                , (x - halfWidth, y - halfHeight)
                                                ]
