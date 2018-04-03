module Visual.Renderer where

import Graphics.Gloss

import Control.Monad (join)
import Control.Arrow ((***))

import Model.CommonTypes
import Util.Constants
import Visual.WindowConstants

-- | Initial window state.
newWindow :: Display
newWindow = InWindow windowName initialWindowDimensions initialWindowPosition


-- | Performs scene rendering inside a window.
render :: Game -> Picture
render game = Pictures $ [positionPicture (gameCamera game) (picture game)] ++ map coinText (zip [0..] $ gamePlayers game)

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
