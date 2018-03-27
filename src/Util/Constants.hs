module Util.Constants where

import Model.CommonTypes

infinity :: Float
infinity = 1/0

epsilon :: Float
epsilon = 0.001

g :: Float
g = 1500

gravitationalVector :: Vector
gravitationalVector = Vector (0, - g)

frictionCoef :: Float
frictionCoef = 1

infiniteRectangle :: Rectangle
infiniteRectangle = (Position (-infinity, -infinity), Position(infinity, infinity))