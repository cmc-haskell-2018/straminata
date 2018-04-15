module Model.LevelPatterns where

import Data.List (isPrefixOf)
import System.IO.Unsafe(unsafePerformIO)
import System.Directory(doesFileExist, getDirectoryContents)

import Model.CommonTypes
import Visual.TextureLoader

levelsNumber :: Int
levelsNumber = id $! length . filter (isPrefixOf "level") . unsafePerformIO . getDirectoryContents $ "level/"

loadFile :: Int -> ([String], String, String, String)
loadFile number = id $! if unsafePerformIO $ doesFileExist filename
                        then format . unsafePerformIO . readFile $ filename
                        else error $ "Level file with number " ++ show number ++ " does not exist."
  where filename = "level/level" ++ show number ++ ".lvl"
        format content =
          let lines' = lines content
              mapSize :: Int
              mapSize = read $ head lines'
          in ( take mapSize (drop 1 lines')
             , lines' !! (mapSize + 1)
             , lines' !! (mapSize + 2)
             , lines' !! (mapSize + 3)
             )

generate :: Float -> [String] -> Map
generate size pattern = map transferLine $ zip [1..] pattern
  where transferLine (y, line) = map (transferSymbol y) $ zip [1..] (words line)
        transferSymbol y (x, 'w' : n) =
          let tex = case n of
                    "1" -> dirtTextureWithGrass
                    "2" -> dirtTextureWithoutGrass
                    _ -> undefined
          in Solid Appearance
            { appearanceBox = (Position (x * size, y * size), Position ((x + 1) * size, (y + 1) * size))
            , appearanceActualSize = fst tex
            , appearanceAnimation = [snd tex]
            }
        transferSymbol y (x, _) = Transparent Appearance
          { appearanceBox = (Position (x * size, y * size), Position ((x + 1) * size, (y + 1) * size))
          , appearanceActualSize = fst transparentTexture
          , appearanceAnimation = [snd transparentTexture]
          }

patterns :: [([String], String, String, String)]
patterns = map loadFile [1..levelsNumber]
