{-# OPTIONS_GHC -Wall #-}
-- | __Straminata__ is a puzzle platform game written completely in Haskell.
module Straminata where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game hiding (Vector)

import WindowConstants
import Model.CommonTypes
import Visual.Renderer
import Visual.TextureLoader
import Util.Common
import Controls

-- | Starts game main loop.
run :: IO ()
run = play
      newWindow
      backgroundColor
      stepsPerSecond
      initialWorld
      render
      handleInput
      advanceGame

-- | Initial game state.
initialWorld :: Game
initialWorld = Game { gameObjects = initialObjects
                    , gamePlayers = [playerInitialState, player2InitialState]
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
      { hitboxPosition = Position (0, 0)
      , hitboxCollisionBoxes = [(Position (0, 0), Position (60, 80))]
      }
    , objectAppearance = Appearance
      { appearanceBox = (Position (0, 0), Position (60, 80))
      , appearanceActualSize = fst marioTexture
      , appearancePicture = snd marioTexture
      }
    , objectVelocity = Vector (0, 0)
  }
  , playerControls =
      [ bindAction (SpecialKey KeyRight) (ControlElement (Vector (1, 0))) (ControlElement (Vector (-1, 0)))
      , bindAction (SpecialKey KeyLeft) (ControlElement (Vector (-1, 0))) (ControlElement (Vector (1, 0)))
      , bindAction (SpecialKey KeyUp) (ControlElement (Vector (0, 1))) (ControlElement (Vector (0, -1)))
      , bindAction (SpecialKey KeyDown) (ControlElement (Vector (0, -1))) (ControlElement (Vector (0, 1)))
      ]
  }

player2InitialState :: Player
player2InitialState = Player
  { playerObject = Object
    { objectName = "Player 2"
    , objectHitbox = Hitbox
      { hitboxPosition = Position (0, 0)
      , hitboxCollisionBoxes = [(Position (0, 0), Position (120, 150))]
      }
    , objectAppearance = Appearance
      { appearanceBox = (Position (0, 0), Position (150, 150))
      , appearanceActualSize = fst luigiTexture
      , appearancePicture = snd luigiTexture
      }
    , objectVelocity = Vector (0, 0)
  }
  , playerControls =
    [ bindAction (Char 'd') (ControlElement (Vector (1, 0))) (ControlElement (Vector (-1, 0)))
    , bindAction (Char 'a') (ControlElement (Vector (-1, 0))) (ControlElement (Vector (1, 0)))
    , bindAction (Char 'w') (ControlElement (Vector (0, 1))) (ControlElement (Vector (0, -1)))
    , bindAction (Char 's') (ControlElement (Vector (0, -1))) (ControlElement (Vector (0, 1)))
    ]
  }

-- | Advances game state one step further.
advanceGame :: Float -- ^ period of time (in seconds) needing to be advanced
            -> Game
            -> Game
-- advanceGame time = updatePlayers . moveObjects time . movePlayers time
advanceGame time = moveObjects time . movePlayers time

-- temporary
updatePlayers :: Game -> Game
updatePlayers game = let playerList = gamePlayers game
                         player1 = (playerList !! 0)
                         player2 = (playerList !! 1)
                         object1 = playerObject player1
                         object2 = playerObject player2
                         playersColliding = hitboxesCollide (objectHitbox object1) (objectHitbox object2)
                         recolorPlayer player = player
                                                { playerObject = (playerObject player)
                                                  { objectAppearance = (objectAppearance . playerObject $ player)
                                                    { appearancePicture =
                                                      (if playersColliding
                                                      then Color red
                                                      else Color blue)
                                                        (Polygon $ formPath
                                                          (hitboxPosition . objectHitbox . playerObject $ player)
                                                          (appearanceBox . objectAppearance . playerObject $ player)
                                                        )
                                                    }
                                                  }
                                                }
                     in game { gamePlayers = map recolorPlayer playerList }
