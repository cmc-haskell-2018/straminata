module Util.Constants where

import Model.CommonTypes

level1TileSize :: Float
level1TileSize = 50

infinity :: Float
infinity = 1/0

epsilon :: Float
epsilon = 0.001

g :: Float
g = 30 * level1TileSize

gravitationalVector :: Vector
gravitationalVector = Vector (0, - g)

frictionCoef :: Float
frictionCoef = 1000 * level1TileSize

infiniteRectangle :: Rectangle
infiniteRectangle = (Position (-infinity, -infinity), Position(infinity, infinity))

maxVelocity :: Float
maxVelocity = 100 * level1TileSize