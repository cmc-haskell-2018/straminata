module Model.Levels where

import Data.List(elemIndex)
import Data.Maybe(fromJust, isJust)
import Data.Tuple(swap)

import Model.CommonTypes
import Model.Objects
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
  , "w t t t t t t t t t t t t t t t t t t t t t t w w w t t t w"
  , "w w t w t w t w t w t w t w t w t w t w t w t t t t t t w w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t w t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t w t t w"
  , "w t t t t t t t t t t t t t t t t t t t t t t t t t t t t w"
  , "w t t t t t t t t t t t b1 t d1 t t t t t t t t w t t t t t w"
  , "w w t t t w t t t t t t w t w t t t t t t t t w t t t t t w"
  , "w w w w w w w w w w w w w w w w w w w w w w w w w w w w w w"
  , "w w w w w w w w w w w w w w w w w w w w w w w w w w w w w w"
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

generateDoors :: Float -> [String] -> [Object]
generateDoors size pattern = foldr (\t acc -> acc ++ transferLine t) [] $ zip [1..] pattern
  where transferLine (y, line) = foldr (
          \x acc -> let t = transferSymbol y x in if isJust t then acc ++ fromJust t else acc)
          [] $ zip [1..] (words line)
        transferSymbol y (x, 'b' : n) =
          let coord = tableIndex ("d" ++ n) pattern
          in Just $ bindButtonAndDoor n
                                      (buttonObject {objectPosition = Position (x * size, y * size)})
                                      (doorObject {objectPosition = Position ( fromIntegral (fst coord) * size, fromIntegral (snd coord) * size)})
        transferSymbol _ _ = Nothing


tableIndex :: String -> [String] -> (Int, Int)
tableIndex e es = swap . withIndex $ filter (\xs -> any (== e) . words $ snd xs) $ zip [1 :: Int ..] es
  where withIndex [(index, xs)] = (index, (fromJust $ elemIndex e $ words xs) + 1)

level1 :: Map
level1 = generate 50 $ reverse level1Pattern

objects1 :: [Object]
objects1 = generateDoors 50 $ reverse level1Pattern
