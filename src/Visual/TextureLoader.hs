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

transparentTexture :: Texture
transparentTexture = ((Dimensions (50, 50)), ) $! png "img/transparent.png"

buttonTexture :: Texture
buttonTexture = ((Dimensions (48, 10)), ) $! png "img/button.png"

buttonPressTexture :: Texture
buttonPressTexture = ((Dimensions (48, 10)), ) $! png "img/button_press.png"

doorOpenTexture :: Texture
doorOpenTexture = ((Dimensions (32, 128)), ) $! png "img/door_open.png"

doorCloseTexture :: Texture
doorCloseTexture = ((Dimensions (32, 128)), ) $! png "img/door_close.png"


