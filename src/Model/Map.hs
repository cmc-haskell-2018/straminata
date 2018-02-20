module Model.Map where

import Model.CommonTypes

initialLevel :: Level
initialLevel = Level
  { levelMap = initialMap
  , levelColNumber = 30
  , levelRowNumber = 40
  , levelTileSize = 50
  }

initialMap :: Map
initialMap = foldr (\index acc -> addMapRow index acc) [] [0..(levelRowNumber initialLevel - 1)]

addMapRow :: Int -> Map -> Map
addMapRow row list = foldr (\index acc -> addMapTile row index acc) [] [0..(levelColNumber initialLevel - 1)] : list

addMapTile :: Int -> Int -> MapRow -> MapRow
addMapTile row col list = let tileSize = levelTileSize initialLevel in
  Transparent Object
    { objectName = show (row, col)
    , objectVelocity = Vector (0, 0)
    , objectHitbox = Hitbox
      { hitboxPosition = Position (fi (tileSize * row), fi (tileSize * col))
      , hitboxDisplayBox = ( Position (0, 0)
                           , Position (fi tileSize, fi tileSize)
                           )
      , hitboxCollisionBoxes = []
      }
    } : list
  where fi = fromIntegral


