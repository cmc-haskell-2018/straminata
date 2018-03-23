module Visual.Renderer where

import Graphics.Gloss

import Visual.WindowConstants
import Model.CommonTypes

-- | Initial window state.
newWindow :: Display
newWindow = InWindow windowName initialWindowDimensions initialWindowPosition


-- | Performs scene rendering inside a window.
render :: Game -> Picture
render game = positionPicture (gameCamera game) (picture game)


-- | Composes all game @objects@ in a list of pictures.
picture :: Game -> Picture
picture game = let translate' = uncurry Translate . unwrap . objectPosition
                   picture' = \o -> translate' o $ objectToPicture o
               in Pictures
                 $ (Scale 5 5 . appearancePicture . levelBackground . gameLevel $ game)
                   : map (tileToPicture) (concat . levelMap . gameLevel $ game)
                   ++ map (picture' . playerObject) (gamePlayers game)
                   ++ map picture' (levelObjects . gameLevel $ game)


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
appearanceToPicture app = --(uncurry Translate . unwrap . fst . appearanceBox $ app) $ --(uncurry Translate translateCoords) $
                          (uncurry Scale $ computeScale (appearanceBox app) (appearanceActualSize app))
                          (appearancePicture app)
--  where
--    translateCoords = (unwrap . fst . appearanceBox $ app) `tupleSum` (invert . divBy2 . unwrap . getDimensions . appearanceBox $ app)
--    tupleSum (a, b) (c, d) = (a + c, b + d)
--    divBy2 (a, b) = (a / 2, b / 2)
--    invert (a, b) = (-a, -b)


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
