{-# OPTIONS_GHC -Wall #-}
-- | __Straminata__ is a puzzle platform game written completely in Haskell.
module Straminata where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game hiding (Vector)

import Model.CommonTypes
import Model.Map
import Visual.WindowConstants
import Visual.Renderer
import Util.Common
import Controls

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

-- | Initial game state.
initialWorld :: Game
initialWorld = Game
  { gameObjects = initialObjects
  , gamePlayers = [playerInitialState, player2InitialState]
  , gameLevel = initialLevel
  , gameCamera = Camera
    { cameraPosition = Position (0, 0)
    , cameraRatio = 1
    }
  }

-- todo: get from file
initialObjects :: [Object]
initialObjects = []

-- todo: get from file
playerInitialState :: Player
playerInitialState = Player
  { playerObject = Object
    { objectName = "Player 1"
    , objectHitbox = Hitbox
      { hitboxPosition = Position (1000, 1000)
      , hitboxDisplayBox = (Position (0, 0), Position (100, 100))
      , hitboxCollisionBoxes = [(Position (0, 0), Position (100, 100))]
      }
    , objectVelocity = Vector (0, 0)
  }
  , playerColor = red
  , playerControls =
      [ bindAction (SpecialKey KeyRight) (ControlElement (Vector (10, 0))) (ControlElement (Vector (-10, 0)))
      , bindAction (SpecialKey KeyLeft) (ControlElement (Vector (-10, 0))) (ControlElement (Vector (10, 0)))
      , bindAction (SpecialKey KeyUp) (ControlElement (Vector (0, 10))) (ControlElement (Vector (0, -10)))
      , bindAction (SpecialKey KeyDown) (ControlElement (Vector (0, -10))) (ControlElement (Vector (0, 10)))
      ]
  }

player2InitialState :: Player
player2InitialState = Player
  { playerObject = Object
    { objectName = "Player 2"
    , objectHitbox = Hitbox
      { hitboxPosition = Position (0, 0)
      , hitboxDisplayBox = (Position (0, 0), Position (200, 200))
      , hitboxCollisionBoxes = [(Position (0, 0), Position (200, 200))]
      }
    , objectVelocity = Vector (0, 0)
  }
  , playerColor = red
  , playerControls =
    [ bindAction (Char 'd') (ControlElement (Vector (20, 0))) (ControlElement (Vector (-20, 0)))
    , bindAction (Char 'a') (ControlElement (Vector (-20, 0))) (ControlElement (Vector (20, 0)))
    , bindAction (Char 'w') (ControlElement (Vector (0, 20))) (ControlElement (Vector (0, -20)))
    , bindAction (Char 's') (ControlElement (Vector (0, -20))) (ControlElement (Vector (0, 20)))
    ]
  }

-- | Advances game state one step further.
advanceGame :: Float -- ^ period of time (in seconds) needing to be advanced
            -> Game
            -> Game
advanceGame time = updatePlayers . updateCamera . moveObjects time . movePlayers time

-- temporary
updatePlayers :: Game -> Game
updatePlayers game = let playerList = gamePlayers game
                         player1 = (playerList !! 0)
                         player2 = (playerList !! 1)
                         h1 = objectHitbox . playerObject $ player1
                         h2 = objectHitbox . playerObject $ player2
                         changeColor = hitboxesCollide h1 h2
                     in game { gamePlayers = map (\player -> player {playerColor = if changeColor then red else blue})
                                                 playerList
                             }
