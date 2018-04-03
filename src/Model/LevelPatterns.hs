module Model.LevelPatterns where

import Model.CommonTypes
import Util.Constants
import Visual.TextureLoader

level1Pattern :: [String]
level1Pattern =
  [ "w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 c1 t1 t1 t1 c1 t1 t1 t1 c1 t1 t1 t1 c1 t1 t1 t1 c1 t1 t1 t1 c1 t1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1 w1 w1 t1 t1 t1 w1"
  , "w1 w1 t1 w1 t1 w1 t1 w1 t1 w1 t1 w1 t1 w1 t1 w1 t1 w1 t1 w1 t1 w1 t1 t1 t1 t1 t1 t1 w1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1 t1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 c1 t1 w1 t1 t1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 c1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 b1 t1 d1 t1 c1 t1 c1 t1 c1 t1 t1 w1 t1 t1 t1 t1 t1 w1"
  , "w1 w1 q2 t1 t1 w1 t1 t1 t1 t1 t1 t1 w1 t1 w1 t1 t1 t1 t1 t1 t1 t1 w1 t1 t1 t1 t1 t1 q1 w1"
  , "w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 t1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1"
  , "w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 t1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1"
  , "w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1"
  ]

level2Pattern :: [String]
level2Pattern =
  [ "w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 t1 t1 t1 t1 d2 b1 t1 t1 t1 b2 d1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 t1 t1 t1 t1 w1 w1 w1 w1 w1 w1 w1 t1 t1 t1 t1 t1 w1"
  , "w1 c1 t1 t1 t1 t1 t1 t1 t1 w1 t1 t1 t1 t1 t1 t1 t1 c1 w1"
  , "w1 b3 t1 c1 t1 c1 d4 t1 t1 w1 t1 t1 d3 c1 t1 c1 t1 b4 w1"
  , "w1 w1 w1 w1 w1 w1 w1 t1 t1 w1 t1 t1 w1 w1 w1 w1 w1 w1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 c1 c1 c1 c1 c1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 t1 w1"
  , "t1 q1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 q2 t1"
  , "w1 w1 w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1 w1 w1"
  ]

level3Pattern :: [String]
level3Pattern =
  [ "w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 c1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 c1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 t1 t1 c1 t1 t1 t1 c1 t1 t1 t1 c1 t1 d1 t1 c1 t1 t1 t1 c1 t1 t1 t1 c1 t1 t1 t1 c1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1 t1 t1 t1 t1 b1 t1 t1 t1 t1 t1 t1 q2 t1 q1 w1"
  , "w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1"
  ]

level4Pattern :: [String]
level4Pattern =
  [ "w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 w1"
  , "w1 t1 t1 c1 c1 t1 c1 c1 t1 c1 c1 t1 c1 c1 t1 t1 c1 c1 t1 c1 c1 t1 c1 c1 t1 c1 c1 t1 c1 w1"
  , "w1 q2 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 t1 q1 w1"
  , "w1 w1 w1 t1 t1 w1 t1 t1 w1 t1 t1 w1 t1 t1 w1 w1 t1 t1 w1 t1 t1 w1 t1 t1 w1 t1 t1 w1 w1 w1"
  ]

generate :: Float -> [String] -> Map
generate size pattern = map transferLine $ zip [1..] pattern
  where transferLine (y, line) = map (transferSymbol y) $ zip [1..] (words line)
        transferSymbol y (x, 'w' : n) =
          let tex = case n of
                    "1" -> floorTexture
                    _ -> undefined
          in Solid Appearance
            { appearanceBox = (Position (x * size, y * size), Position ((x + 1) * size, (y + 1) * size))
            , appearanceActualSize = fst tex
            , appearancePicture = snd tex
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

level3Map :: Map
level3Map = generate level1TileSize $ reverse level3Pattern

level4Map :: Map
level4Map = generate level1TileSize $ reverse level4Pattern
