module Model.Map where

import Control.Monad (join)
import Control.Arrow ((***))

import Model.CommonTypes
import Visual.TextureLoader
import Visual.WindowConstants
import Util.Common

initialObjects :: [Object]
initialObjects = []

initialLevel :: Level
initialLevel = Level
  { levelMap = initialMap
  , levelColNumber = 30
  , levelRowNumber = 40
  , levelTileSize = 50
  , levelObjects = initialObjects
  , levelBackground = Appearance
    { appearanceBox = (join (***)) (Position . fromIntegral) initialWindowDimensions
    , appearancePicture = snd backgroundTexture
    , appearanceActualSize = fst backgroundTexture
    }
  }

initialMap :: Map
initialMap = foldr (\index acc -> addMapRow index acc) [] [0..(levelRowNumber initialLevel - 1)]

addMapRow :: Int -> Map -> Map
addMapRow row list = foldr (\index acc -> addMapTile row index acc) [] [0..(levelColNumber initialLevel - 1)] : list

addMapTile :: Int -> Int -> MapRow -> MapRow
addMapTile row col list = let tileSize = levelTileSize initialLevel in
  Tile
  { tileType = Solid
  , tileObject = Object
    { objectName = show (row, col)
    , objectVelocity = Vector (0, 0)
    , objectPosition = Position (tileSize * (fi row), tileSize * (fi col))
    , objectCollisionBoxes = []
    , objectAppearance = Appearance
            { appearanceBox = (Position (0, 0), Position (tileSize, tileSize))
            , appearanceActualSize = fst floorTexture
            , appearancePicture = snd floorTexture
            }
    }
  } : list
  where fi = fromIntegral


