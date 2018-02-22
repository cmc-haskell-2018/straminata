{-# LANGUAGE TupleSections #-}
module Visual.TextureLoader where

import Graphics.Gloss.Data.Picture (Picture)
import Graphics.Gloss.Game (png)

import Model.CommonTypes

floorTile :: (Dimensions, Picture)
floorTile = ((Dimensions (400, 400)), ) $! png "img/floor.png"
