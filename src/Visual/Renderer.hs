module Visual.Renderer where

import Graphics.Gloss

import WindowConstants
import Model.CommonTypes

-- | Initial window state.
newWindow :: Display
newWindow = InWindow windowName initialWindowDimensions initialWindowPosition


-- | Performs scene rendering inside a window.
render :: Game -> Picture
render = picture


-- | Composes all game @objects@ in a list of pictures.
picture :: Game -> Picture
picture game = let translate' = uncurry Translate . unwrap . hitboxPosition . objectHitbox
                   scale' = \o -> uncurry Scale $ computeScale (appearanceBox . objectAppearance $ o)
                                                               (appearanceActualSize . objectAppearance $ o)
                   picture' = \o -> translate' o $ scale' o $ appearancePicture (objectAppearance o)
               in Pictures
                 $ map (picture' . playerObject) (gamePlayers game)
                   ++ map picture' (gameObjects game)


-- | Creates a path – a sequential list of polygon vertices.
formPath :: Position -> Rectangle -> [(Float, Float)]
formPath pos box = let x = fst . unwrap $ pos
                       y = snd . unwrap $ pos
                       p1 = unwrap . fst $ box
                       p2 = unwrap . snd $ box
                       x1 = fst p1 + x
                       y1 = snd p1 + y
                       x2 = fst p2 + x
                       y2 = snd p2 + y
                   in [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]
