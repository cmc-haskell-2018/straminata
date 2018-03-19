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
                                          then addBasicMapTile (True, floorTexture) row index acc
                                          else addBasicMapTile (False, backgroundTexture) row index acc)
                           []
                           [0..(levelColNumber initialLevel - 1)] : list

-- | Adds basic map tile to map tile row. Basic map tile has a single rectangle as a collision box.
addBasicMapTile :: (Bool, Texture)
                -> Int
                -> Int
                -> MapRow
                -> MapRow
addBasicMapTile (isSolid, texture) row col list =
  ( if isSolid
    then Solid appearance
    else Transparent appearance
  ) : list
  where tileSize = levelTileSize initialLevel
        fi = fromIntegral
        appearance =
          Appearance
            { appearanceBox =
                ( Position (tileSize * (fi row), tileSize * (fi col))
                , Position (tileSize * (fi row) + tileSize, tileSize * (fi col) + tileSize)
                )
            , appearanceActualSize = fst texture
            , appearancePicture = snd texture
            }


