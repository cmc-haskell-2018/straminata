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
picture game = let translate' = uncurry Translate . unwrap . hitboxPosition . objectHitbox
                   polygon' = Polygon . formPath . objectHitbox
                   picture' = \o -> translate' o $ polygon' o
               in Pictures
                 $ map (Color green . picture' . unwrapMapTile) (concat . levelMap . gameLevel $ game)
                   ++ map (\player -> Color (playerColor player) (picture' . playerObject $ player)) (gamePlayers game)
                   ++ map (Color blue . picture') (gameObjects game)


-- | Creates a path – a sequential list of polygon vertices.
formPath :: Hitbox -> [(Float, Float)]
formPath h = let box = hitboxDisplayBox h
                 p1 = unwrap . fst $ box
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
