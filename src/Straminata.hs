{-# OPTIONS_GHC -Wall #-}
-- | __Straminata__ is a puzzle platform game written completely in Haskell.
module Straminata where

import Graphics.Gloss

import System.Environment(getArgs)

import Model.CommonTypes
import Model.Objects
import Util.Common
import Util.Controls
import Visual.WindowConstants
import Visual.Renderer
import Generator

-- | Starts game main loop.
run :: IO ()
run = getArgs >>= startMachine . parseMode

startMachine :: Mode -> IO ()
startMachine GameRunner = play
                          newWindow
                          backgroundColor
                          fps
                          initialWorld
                          render
                          handleInput
                          advanceGame
startMachine GeneratorRunner = play
                         newWindow
                         white
                         fps
                         initialGeneratorState
                         renderGenerator
                         handleGeneratorInput
                         advanceGenerator

-- | Advances game state one step further.
advanceGame :: Float -- ^ period of time (in seconds) needing to be advanced
            -> Game
            -> Game
advanceGame time = updateCamera . updateAnimations . updatePhysics time . updateObjects
