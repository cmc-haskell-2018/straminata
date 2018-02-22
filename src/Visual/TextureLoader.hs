{-# LANGUAGE TupleSections #-}
module Visual.TextureLoader where

import Graphics.Gloss.Data.Picture (Picture)
import Graphics.Gloss.Game (png)

import Model.CommonTypes

floorTexture :: (Dimensions, Picture)
floorTexture = ((Dimensions (400, 400)), ) $! png "img/floor.png"

marioTexture :: (Dimensions, Picture)
marioTexture = ((Dimensions (300, 400)), ) $! png "img/mario.png"

luigiTexture :: (Dimensions, Picture)
luigiTexture = ((Dimensions (420, 420)), ) $! png "img/luigi.png"
