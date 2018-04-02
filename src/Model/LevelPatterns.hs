module Model.LevelPatterns where

import Model.CommonTypes
import Util.Constants
import Visual.TextureLoader

level1Pattern :: [String]
level1Pattern =
  [ "w w w w w w w w w w w w w w w w w w w w w w w w w w w w w w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w q t t t t t t t t t t t t t t t t t t t t t w w w t t t w"
  , "w w t w t w t w t w t w t w t w t w t w t w t t t t t t w w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t w t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t w t t w"
  , "w t t t t c t c c t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t b1 t d1 t t t t t t t t w t t t t t w"
  , "w w t t t w t t t t t t w t w t t t t t t t w t t t t t t w"
  , "w w w w w w w w w w w w w w w w w w w w w w w w w w w w w w"
  , "w w w w w w w w w w w w w w w w w w w w w w w w w w w w w w"
  , "w w w w w w w w w w w w w w w w w w w w w w w w w w w w w w"
  ]

level2Pattern :: [String]
level2Pattern =
  [ "w w w w w w w w w w w w w w w w w w w w w w w w w w w w w w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t c t c t c t c t c t c t c t c t c t c t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t q w"
  , "w w w w w w w w w w w w w w w w w w w w w w w w w w w w w w"
  ]

generate :: Float -> [String] -> Map
generate size pattern = map transferLine $ zip [1..] pattern
  where transferLine (y, line) = map (transferSymbol y) $ zip [1..] (words line)
        transferSymbol y (x, "w") = Solid Appearance
          { appearanceBox = (Position (x * size, y * size), Position ((x + 1) * size, (y + 1) * size))
          , appearanceActualSize = fst floorTexture
          , appearancePicture = snd floorTexture
          }
        transferSymbol y (x, _) = Transparent Appearance
          { appearanceBox = (Position (x * size, y * size), Position ((x + 1) * size, (y + 1) * size))
          , appearanceActualSize = fst transparentTexture
          , appearancePicture = snd transparentTexture
          }

level1Map :: Map
level1Map = generate level1TileSize $ reverse level1Pattern

level2Map :: Map
level2Map = generate level1TileSize $ reverse level2Pattern
