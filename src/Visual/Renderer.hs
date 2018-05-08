module Visual.Renderer where

import Graphics.Gloss

import Control.Monad (join)
import Control.Arrow ((***))
import Foreign.Marshal.Unsafe(unsafeLocalState)

import Model.CommonTypes
import Util.Constants
import Visual.WindowConstants
import Visual.TextureLoader

-- | Initial window state.
newWindow :: Display
newWindow = InWindow windowName initialWindowDimensions initialWindowPosition


-- | Performs scene rendering inside a window.
render :: Game -> Picture
render game 
  | rules == True = Pictures $ [positionPicture (gameCamera game) (picture game)] 
                           ++ (map coinText (zip [0..] $ gamePlayers game)) 
                           ++ drawRules
  | start == True = Pictures $ [positionPicture (gameCamera game) (picture game)] 
                           ++ (map coinText (zip [0..] $ gamePlayers game)) 
                           ++ drawStartLevel ++ drawTextStart game
  | otherwise = Pictures $ [positionPicture (gameCamera game) (picture game)] ++ map coinText (zip [0..] $ gamePlayers game)
  where start = levelStart $ gameLevel game
        rules = gameRules game


appearanceRules :: Appearance
appearanceRules = Appearance
    { appearanceBox = (Position (0, 0), Position (600, 400))
    , appearanceActualSize = fst rulesTexture
    , appearanceAnimation = [snd rulesTexture]
    }


drawRules :: [Picture]
drawRules = let translate' = uncurry Translate . unwrap $ Position (0, 0)
                  in [translate' $ appearanceToPicture appearanceRules]


appearanceStart :: Appearance
appearanceStart = Appearance
    { appearanceBox = (Position (0, 0), Position (600, 400))
    , appearanceActualSize = fst startTexture
    , appearanceAnimation = [snd startTexture]
    }


drawTextStart :: Game -> [Picture]
drawTextStart game 
  | levelNumber level == 1 = [ uncurry Translate (x - 60, y) (Scale t t (Color red (Text ("Hey! Welcome to the game Straminata\n")))) ]
  | otherwise = [Pictures 
                        [ uncurry Translate (x, y) (Scale t t (Color orange (Text (linesStart !! 0))))
                        , uncurry Translate (x - 60, y - 30) (Scale t t (Color violet (Text (linesStart !! 1))))
                        , uncurry Translate (x - 20, y - 60) (Scale t t (Color red (Text (linesStart !! 2))))
                        , uncurry Translate (x - 20, y - 90) (Scale t t (Color blue (Text (linesStart !! 3))))
                        ]
                ]
  where linesStart = unsafeLocalState $ (do
                                          textStart <- readFile "dataLevel.txt"
                                          return (lines textStart))
        level = gameLevel game
        x = -190
        y = 40
        t = 0.2 


drawStartLevel :: [Picture]
drawStartLevel = let translate' = uncurry Translate . unwrap $ Position (0, 0)
                 in [translate' $ appearanceToPicture appearanceStart]


coinText :: (Int, Player) -> Picture
coinText (n, p) = uncurry Translate ( plusPos (fromIntegral n)
                                    $ join (***) (negate . fromIntegral . (`div` 2)) initialWindowDimensions)
             $ Scale 0.3 0.3
             $ Text (name p ++ ":" ++ show (playerCoins p))
  where name = objectName . playerObject
        plusPos n' (x, y) = (x + 10, y + 15 + n' * 40)


-- | Composes all game @objects@ in a list of pictures.
picture :: Game -> Picture
picture game = let translate' = uncurry Translate . unwrap . objectPosition
                   picture' = \o -> translate' o $ objectToPicture o
               in Pictures
                 $ (Scale (level1TileSize / 30) (level1TileSize / 30) . head . appearanceAnimation . levelBackground . gameLevel $ game)
                   : map (tileToPicture) (concat . levelMap . gameLevel $ game)
                   ++ map picture' (levelObjects . gameLevel $ game)
                   ++ map (picture' . playerObject) (gamePlayers game)


-- | Creates a path – a sequential list of polygon vertices.
formPath :: Rectangle -> [(Float, Float)]
formPath box = let p1 = unwrap . fst $ box
                   p2 = unwrap . snd $ box
                   x1 = fst p1
                   y1 = snd p1
                   x2 = fst p2
                   y2 = snd p2
               in [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]


positionPicture :: Camera -> Picture -> Picture
positionPicture camera pic = Scale ratio ratio (Translate x y pic)
  where ratio = cameraRatio camera
        offset = unwrap . invertVector . positionToVector . cameraPosition $ camera
        x = fst offset
        y = snd offset


appearanceToPicture :: Appearance -> Picture
appearanceToPicture app = (uncurry Scale $ computeScale (appearanceBox app) (appearanceActualSize app))
                          (head . appearanceAnimation $ app)


objectToPicture :: Object -> Picture
objectToPicture obj = translate' obj $ appearanceToPicture . objectAppearance $ obj
  where getOffset o = (plus (positionToVector . fst . appearanceBox . objectAppearance $ o)
                            (positionToVector . snd . appearanceBox . objectAppearance $ o)
                      ) `divByNumber` 2
        translate' = uncurry Translate . unwrap . getOffset


tileToPicture :: Tile -> Picture
tileToPicture tile = (translate' $ getAppearance tile) $ appearanceToPicture . getAppearance $ tile
  where getOffset app = (plus (positionToVector . fst . appearanceBox $ app)
                              (positionToVector . snd . appearanceBox $ app)
                        ) `divByNumber` 2
        translate' = uncurry Translate . unwrap . getOffset
