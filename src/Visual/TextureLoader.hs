{-# LANGUAGE TupleSections #-}

module Visual.TextureLoader where

import Graphics.Gloss (Picture)
import Graphics.Gloss.Game (png)

import Model.CommonTypes

floorTexture :: Texture
floorTexture = ((Dimensions (400, 400)), ) $! png "img/floor.png"

marioTexture :: Texture
marioTexture = ((Dimensions (30, 40)), ) $! png "img/mario.png"

luigiTexture1 :: Texture
luigiTexture1 = ((Dimensions (30, 40)), ) $! png "img/m1.png"

luigiTexture2 :: Texture
luigiTexture2 = ((Dimensions (30, 40)), ) $! png "img/m2.png"

jTexture1 :: Texture
jTexture1 = ((Dimensions (50, 50)), ) $! png "img/j2.png"

jTexture2 :: Texture
jTexture2 = ((Dimensions (50, 50)), ) $! png "img/j3.png"

upAnimation :: [Picture]
upAnimation = timerAnimationControl2 1

luigiStandingAnimation :: [Picture]
luigiStandingAnimation = timerAnimationControl 1

timerAnimationControl :: Int -> [Picture]
timerAnimationControl  i = if i <  30
                           then snd luigiTexture1 : timerAnimationControl ((i + 1) `mod` 60)
                           else snd luigiTexture2 : timerAnimationControl ((i + 1) `mod` 60)

timerAnimationControl2 :: Int -> [Picture]
timerAnimationControl2  i = (if i <  15
                            then snd luigiTexture1 -- : timerAnimationControl2 ((i + 1) `mod` 60)
                            else if i < 30
                                then snd jTexture2
                                else if i < 45
                                     then snd jTexture1 -- : timerAnimationControl2 ((i + 1) `mod` 60)
                                     else snd luigiTexture2
                            ) : timerAnimationControl2 ((i + 1) `mod` 60)

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

coinTexture :: Texture
coinTexture = ((Dimensions (256, 256)), ) $! png "img/coin.png"

teleport1Texture :: Texture
teleport1Texture = ((Dimensions (32, 128)), ) $! png "img/teleport1.png"

teleport2Texture :: Texture
teleport2Texture = ((Dimensions (32, 128)), ) $! png "img/teleport2.png"
