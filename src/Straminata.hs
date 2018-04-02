{-# OPTIONS_GHC -Wall #-}
-- | __Straminata__ is a puzzle platform game written completely in Haskell.
module Straminata where

import Graphics.Gloss

import Model.CommonTypes
import Model.Objects
import Util.Common
import Util.Controls
import Visual.WindowConstants
import Visual.Renderer

-- | Starts game main loop.
run :: IO ()
run = play
      newWindow
      backgroundColor
      fps
      initialWorld
      render
      handleInput
      advanceGame

-- | Advances game state one step further.
advanceGame :: Float -- ^ period of time (in seconds) needing to be advanced
            -> Game
            -> Game
advanceGame time = updateCamera . updatePhysics time . updateObjects
