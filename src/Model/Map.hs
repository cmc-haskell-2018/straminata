module Model.Map where

import Control.Monad (join)
import Control.Arrow ((***))

import Model.CommonTypes
import Visual.TextureLoader
import Visual.WindowConstants

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
    { appearanceBox = (Position (0, 0), Position . (join (***)) (fromIntegral) $ initialWindowDimensions)
    , appearancePicture = snd backgroundTexture
    , appearanceActualSize = fst backgroundTexture
    }
  }

initialMap :: Map
initialMap = foldr (\index acc -> addMapRow index acc) [] [0..(levelRowNumber initialLevel - 1)]

addMapRow :: Int -> Map -> Map
addMapRow row list = foldr (\index acc -> if even index
                                          then addBasicMapTile (Solid, floorTexture) row index acc
                                          else addBasicMapTile (Transparent, backgroundTexture) row index acc)
                           []
                           [0..(levelColNumber initialLevel - 1)] : list

-- | Adds basic map tile to map tile row. Basic map tile has a single rectangle as a collision box.
addBasicMapTile :: (TileType, Texture)
           -> Int
           -> Int
           -> MapRow
           -> MapRow
addBasicMapTile (typ, texture) row col list = let tileSize = levelTileSize initialLevel in
  Tile
  { tileType = typ
  , tileObject = Object
    { objectName = show (row, col) -- Should be unique
    , objectVelocity = Vector (0, 0)
    , objectPosition = Position (tileSize * (fi row), tileSize * (fi col))
    , objectCollisionBoxes = [(Position (0, 0), Position (tileSize, tileSize))]
    , objectAppearance = Appearance
      { appearanceBox = (Position (0, 0), Position (tileSize, tileSize))
      , appearanceActualSize = fst texture
      , appearancePicture = snd texture
      }
    }
  } : list
  where fi = fromIntegral


