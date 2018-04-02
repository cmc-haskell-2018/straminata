{-# LANGUAGE TupleSections #-}

module Visual.TextureLoader where

import Graphics.Gloss (Picture)
import Graphics.Gloss.Game (png)

import Model.CommonTypes

floorTexture :: Texture
floorTexture = ((Dimensions (400, 400)), ) $! png "img/tile.png"


-- | exa idle
exaIdle1 :: Texture
exaIdle1 = ((Dimensions (100, 100)), ) $! png "img/exa/idle/id1.png"

exaIdle2 :: Texture
exaIdle2 = ((Dimensions (100, 100)), ) $! png "img/exa/idle/id2.png"

exaIdle3 :: Texture
exaIdle3 = ((Dimensions (100, 100)), ) $! png "img/exa/idle/id3.png"

-- | exa walk

exaWalk2 :: Texture
exaWalk2 = ((Dimensions (100, 100)), ) $! png "img/exa/walk/wa_2.png"

exaWalk3 :: Texture
exaWalk3 = ((Dimensions (100, 100)), ) $! png "img/exa/walk/wa_3.png"

exaWalk4 :: Texture
exaWalk4 = ((Dimensions (100, 100)), ) $! png "img/exa/walk/wa_4.png"

exaWalk5 :: Texture
exaWalk5 = ((Dimensions (100, 100)), ) $! png "img/exa/walk/wa_5.png"

-- | exa jump

exaJump2 :: Texture
exaJump2 = ((Dimensions (100, 100)), ) $! png "img/exa/jump/jp_2.png"

exaJump3 :: Texture
exaJump3 = ((Dimensions (100, 100)), ) $! png "img/exa/jump/jp_3.png"

exaJump4 :: Texture
exaJump4 = ((Dimensions (100, 100)), ) $! png "img/exa/jump/jp_4.png"

exaJump5 :: Texture
exaJump5 = ((Dimensions (100, 100)), ) $! png "img/exa/jump/jp_5.png"

exaJump6 :: Texture
exaJump6 = ((Dimensions (100, 100)), ) $! png "img/exa/jump/jp_6.png"

exaIdleAnimation :: [Picture]
exaIdleAnimation = exaIdleAnimationControl 1

exaIdleAnimationControl :: Int -> [Picture]
exaIdleAnimationControl  i = (if i <  30
                           then snd exaIdle1
                           else if i < 35
                                then snd exaIdle2
                                else if i < 40
                                     then snd exaIdle3
                                     else snd exaIdle1
                           ) : exaIdleAnimationControl ((i + 1) `mod` 60)

exaJumpAnimation :: [Picture]
exaJumpAnimation = exaJumpAnimationControl 1

exaJumpAnimationControl :: Int -> [Picture]
exaJumpAnimationControl  i = (if i <  10
                         then snd exaIdle1
                         else  if i < 20
                               then snd exaJump2
                               else if i < 30
                                    then snd exaJump3
                                    else if i < 40
                                         then snd exaJump4
                                         else if i < 50
                                              then snd exaJump5
                                              else snd exaJump6
                           ) : exaJumpAnimationControl ((i + 1) `mod` 60)


exaWalkAnimation :: [Picture]
exaWalkAnimation = exaWalkAnimationControl 1

exaWalkAnimationControl :: Int -> [Picture]
exaWalkAnimationControl  i = (if i <  10
                           then snd exaIdle1
                           else if i < 25
                                then snd exaWalk2
                                else if i < 35
                                     then snd exaWalk3
                                     else if i < 500
                                          then snd exaWalk4
                                          else snd exaWalk5
                           ) : exaWalkAnimationControl ((i + 1) `mod` 60)

-- | ine idle
ineIdle1 :: Texture
ineIdle1 = ((Dimensions (100, 100)), ) $! png "img/ine/idle/id_1.png"

ineIdle2 :: Texture
ineIdle2 = ((Dimensions (100, 100)), ) $! png "img/ine/idle/id_2.png"

ineIdle3 :: Texture
ineIdle3 = ((Dimensions (100, 100)), ) $! png "img/ine/idle/id_3.png"

-- | ine walk
ineWalk1 :: Texture
ineWalk1 = ((Dimensions (100, 100)), ) $! png "img/ine/walk/wa_1.png"

ineWalk2 :: Texture
ineWalk2 = ((Dimensions (100, 100)), ) $! png "img/ine/walk/wa_2.png"

ineWalk3 :: Texture
ineWalk3 = ((Dimensions (100, 100)), ) $! png "img/ine/walk/wa_3.png"

ineWalk4 :: Texture
ineWalk4 = ((Dimensions (100, 100)), ) $! png "img/ine/walk/wa_4.png"

ineWalk5 :: Texture
ineWalk5 = ((Dimensions (100, 100)), ) $! png "img/ine/walk/wa_5.png"

ineWalk6 :: Texture
ineWalk6 = ((Dimensions (100, 100)), ) $! png "img/ine/walk/wa_6.png"

-- | ine jump
ineJump1 :: Texture
ineJump1 = ((Dimensions (100, 100)), ) $! png "img/ine/jump/jp_1.png"

ineJump2 :: Texture
ineJump2 = ((Dimensions (100, 100)), ) $! png "img/ine/jump/jp_2.png"

ineJump3 :: Texture
ineJump3 = ((Dimensions (100, 100)), ) $! png "img/ine/jump/jp_3.png"

ineJump4 :: Texture
ineJump4 = ((Dimensions (100, 100)), ) $! png "img/ine/jump/jp_4.png"

ineJump5 :: Texture
ineJump5 = ((Dimensions (100, 100)), ) $! png "img/ine/jump/jp_5.png"

ineJump6 :: Texture
ineJump6 = ((Dimensions (100, 100)), ) $! png "img/ine/jump/jp_6.png"

ineIdleAnimation :: [Picture]
ineIdleAnimation = ineIdleAnimationControl 1

ineIdleAnimationControl :: Int -> [Picture]
ineIdleAnimationControl  i = (if i <  15
                           then snd ineIdle1
                           else if i < 20
                                then snd ineIdle2
                                else if i < 25
                                     then snd ineIdle3
                                     else snd ineIdle1
                           ) : ineIdleAnimationControl ((i + 1) `mod` 60)

ineJumpAnimation :: [Picture]
ineJumpAnimation = ineJumpAnimationControl 1

ineJumpAnimationControl :: Int -> [Picture]
ineJumpAnimationControl  i = (if i <  5
                           then snd ineIdle1
                           else if i < 10
                                then snd ineJump1
                                else if i < 20
                                     then snd ineJump2
                                     else if i < 30
                                          then snd ineJump3
                                          else if i < 40
                                               then snd ineJump4
                                               else if i < 50
                                                    then snd ineJump5
                                                    else snd ineJump6
                           ) : ineJumpAnimationControl ((i + 1) `mod` 60)


ineWalkAnimation :: [Picture]
ineWalkAnimation = ineWalkAnimationControl 1

ineWalkAnimationControl :: Int -> [Picture]
ineWalkAnimationControl  i = (if i <  5
                           then snd ineIdle1
                           else if i < 10
                                then snd ineWalk1
                                else if i < 20
                                     then snd ineWalk2
                                     else if i < 30
                                          then snd ineWalk3
                                          else if i < 40
                                               then snd ineWalk4
                                               else if i < 50
                                                    then snd ineWalk5
                                                    else snd ineWalk6
                           ) : ineWalkAnimationControl ((i + 1) `mod` 60)





backgroundTexture :: Texture
backgroundTexture = ((Dimensions (1920, 1080)), ) $! png "img/bckgrnd.png"

transparentTexture :: Texture
transparentTexture = ((Dimensions (50, 50)), ) $! png "img/transparent.png"

buttonTexture :: Texture
buttonTexture = ((Dimensions (48, 20)), ) $! png "img/button.png"

buttonPressTexture :: Texture
buttonPressTexture = ((Dimensions (48, 20)), ) $! png "img/button_pressed.png"

doorOpenTexture :: Texture
doorOpenTexture = ((Dimensions (32, 128)), ) $! png "img/door_opened.png"

doorCloseTexture :: Texture
doorCloseTexture = ((Dimensions (32, 128)), ) $! png "img/door.png"

coinTexture :: Texture
coinTexture = ((Dimensions (256, 256)), ) $! png "img/coinreel.png"

teleport1Texture :: Texture
teleport1Texture = ((Dimensions (32, 128)), ) $! png "img/teleport1.png"

teleport2Texture :: Texture
teleport2Texture = ((Dimensions (32, 128)), ) $! png "img/teleport2.png"
