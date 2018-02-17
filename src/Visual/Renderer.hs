module Visual.Renderer where

import Graphics.Gloss

import WindowConstants
import Model.CommonTypes
import Util.Common

-- | Initial window state.
newWindow :: Display
newWindow = InWindow windowName initialWindowDimensions initialWindowPosition


-- | Performs scene rendering inside a window.
render :: Game -> Picture
render = picture


-- | Composes all game @objects@ in a list of pictures.
picture :: Game -> Picture
picture game = let player1 = firstPlayer . players $ game
                   player2 = secondPlayer . players $ game
                   translate' = uncurry Translate . unwrap . position . hitbox
                   polygon' = Polygon . formPath . hitbox
                   picture' = \o -> translate' o $ polygon' o
               in Pictures $ (Color (playerColor player1) (picture' (object player1)))
                           : (Color (playerColor player2) (picture' (object player2)))
                           : map picture' (objects game)


-- | Creates a path – a sequential list of polygon vertices.
formPath :: Hitbox -> [(Float, Float)]
formPath h = let pos = position h
                 box = displayBox h
                 x = fst . unwrap $ pos
                 y = snd . unwrap $ pos
                 p1 = unwrap . fst . unwrapRectangle $ box
                 p2 = unwrap . snd . unwrapRectangle $ box
                 x1 = fst p1 + x
                 y1 = snd p1 + y
                 x2 = fst p2 + x
                 y2 = snd p2 + y
             in [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]
