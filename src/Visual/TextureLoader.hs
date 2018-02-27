{-# LANGUAGE TupleSections #-}
module Visual.TextureLoader where

import Graphics.Gloss.Game (png)

import Model.CommonTypes

floorTexture :: Texture
floorTexture = ((Dimensions (400, 400)), ) $! png "img/floor.png"

marioTexture :: Texture
marioTexture = ((Dimensions (30, 40)), ) $! png "img/mario.png"

luigiTexture :: Texture
luigiTexture = ((Dimensions (30, 40)), ) $! png "img/luigi.png"

backgroundTexture :: Texture
backgroundTexture = ((Dimensions (1920, 1080)), ) $! png "img/background.png"
