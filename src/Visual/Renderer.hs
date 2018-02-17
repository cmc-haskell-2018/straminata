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
                   scale' = uncurry Scale . unwrap . scaling . hitbox
                   polygon' = Polygon . formPath . hitbox
                   picture' = \o -> translate' o $ scale' o $ polygon' o
               in Pictures $ (Color (playerColor player1) (picture' (object player1)))
                           : (Color (playerColor player2) (picture' (object player2)))
                           : map picture' (objects game)
