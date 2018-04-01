module Model.Levels where

import Model.CommonTypes
import Visual.TextureLoader

level1Pattern :: [String]
level1Pattern =
  [ "wwwwwwwwwwwwwwwwwwwwwwwwwwwwww"
  , "wttttttttttttttttttttttttttttw"
  , "wttttttttttttttttttttttttttttw"
  , "wttttttttttttttttttttttttttttw"
  , "wttttttttttttttttttttttttttttw"
  , "wttttttttttttttttttttttttttttw"
  , "wttttttttttttttttttttttttttttw"
  , "wttttttttttttttttttttttttttttw"
  , "wttttttttttttttttttttttttttttw"
  , "wttttttttttttttttttttttttttttw"
  , "wttttttttttttttttttttttttttttw"
  , "wttttttttttttttttttttttttttttw"
  , "wttttttttttttttttttttttttttttw"
  , "wttttttttttttttttttttttttttttw"
  , "wttttttttttttttttttttttttttttw"
  , "wttttttttttttttttttttttttttttw"
  , "wttttttttttttttttttttttttttttw"
  , "wttttttttttttttttttttttttttttw"
  , "wttttttttttttttttttttttttttttw"
  , "wttttttttttttttttttttttttttttw"
  , "wttttttttttttttttttttttttttttw"
  , "wttttttttttttttttttttttttttttw"
  , "wttttttttttttttttttttttwwwtttw"
  , "wwtwtwtwtwtwtwtwtwtwtwttttttww"
  , "wttttttttttttttttttttttttttwtw"
  , "wtttttttttttttttttttttttttwttw"
  , "wttttttttttttttttttttttttwtttw"
  , "wtttttttttttttttttttttttwttttw"
  , "wttttttttttttttttttttttwtttttw"
  , "wwwwwwwwwwwwwwwwwwwwwwwwwwwwww"
  ]

generate :: Float -> [String] -> Map
generate size pattern = map transferLine $ zip [1..] pattern
  where transferLine (y, line) = map (transferSymbol y) $ zip [1..] line
        transferSymbol y (x, 'w') = Solid Appearance
          { appearanceBox = (Position (x * size, y * size), Position ((x + 1) * size, (y + 1) * size))
          , appearanceActualSize = fst floorTexture
          , appearancePicture = snd floorTexture
          }
        transferSymbol y (x, 't') = Transparent Appearance
          { appearanceBox = (Position (x * size, y * size), Position ((x + 1) * size, (y + 1) * size))
          , appearanceActualSize = fst transparentTexture
          , appearancePicture = snd transparentTexture
          }

level1 :: Map
level1 = generate 50 $ reverse level1Pattern