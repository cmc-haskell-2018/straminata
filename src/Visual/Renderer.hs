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
picture game = Pictures $ map (\object ->
                               uncurry Translate (unwrap $ position.hitbox $ object)
                               $ uncurry Scale (unwrap $ scaling.hitbox $ object)
                               $ Polygon (formPath $ hitbox object))
                        $ objects game
