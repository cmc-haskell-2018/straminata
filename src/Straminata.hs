{-# OPTIONS_GHC -Wall #-}
-- | __Straminata__ is a puzzle platform game written completely in Haskell.
module Straminata where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game hiding (Vector)

import Model.CommonTypes
import Model.Map
import Util.Common
import Util.Controls
import Util.Constants
import Visual.WindowConstants
import Visual.Renderer
import Visual.TextureLoader

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
  { gamePlayers = [playerInitialState, player2InitialState]
  , gameLevel = initialLevel
  , gameCamera = Camera
    { cameraPosition = Position (0, 0)
    , cameraRatio = 1
    }
  }

marioControls1 :: PlayerControls
marioControls1 =
  [ bindAction (SpecialKey KeyRight) (movePlayer (Vector (level1TileSize * 4, 0))) (movePlayer (Vector (-level1TileSize * 4, 0)))
  , bindAction (SpecialKey KeyLeft) (movePlayer (Vector (-level1TileSize * 4, 0))) (movePlayer (Vector (level1TileSize * 4, 0)))
  , bindAction (SpecialKey KeyUp) (jumpPlayer (Vector (0, level1TileSize * 10))) (zeroAction)
  , bindAction (Char 'c') (setAffectionByGravity False) (switchControlsAction marioControls2)
  , bindAction (SpecialKey KeyEnter) (activateObject True) (zeroAction)
  , bindAction (Char '/') (activateObject False) (zeroAction)
  ]

marioControls2 :: PlayerControls
marioControls2 =
  [ bindAction (SpecialKey KeyRight) (flightPlayer (Vector (level1TileSize * 10, 0))) (stopFlightPlayer (Vector (level1TileSize * 10, 0)))
  , bindAction (SpecialKey KeyLeft) (flightPlayer (Vector (-level1TileSize * 10, 0))) (stopFlightPlayer (Vector (-level1TileSize * 10, 0)))
  , bindAction (SpecialKey KeyUp) (flightPlayer (Vector (0, level1TileSize * 10))) (stopFlightPlayer (Vector (0, level1TileSize * 10)))
  , bindAction (SpecialKey KeyDown) (flightPlayer (Vector (0, -level1TileSize * 10))) (stopFlightPlayer (Vector (0, -level1TileSize * 10)))
  , bindAction (SpecialKey KeySpace) (stopPlayer) (zeroAction)
  , bindAction (Char 'c') (setAffectionByGravity True) (switchControlsAction marioControls1)
  , bindAction (SpecialKey KeyEnter) (activateObject True) (zeroAction)
  , bindAction (Char '/') (activateObject False) (zeroAction)
  ]


-- todo: get from file
playerInitialState :: Player
playerInitialState = Player
  { playerObject = Object
    { objectName = "mario"
    , objectPosition = Position (level1TileSize * 2, level1TileSize * 8)
    , objectCollisionBoxes = [(Position (level1TileSize / 5 * 4, 0), Position (level1TileSize / 5 * 6, level1TileSize / 5 * 8))]
    , objectAppearance = Appearance
      { appearanceBox = (Position (level1TileSize / 5 * 4, 0), Position (level1TileSize / 5 * 6, level1TileSize / 5 * 8))
      , appearanceActualSize = fst marioTexture
      , appearancePicture = snd marioTexture
      }
    , objectVelocity = Vector (0, 0)
    , objectOnUpdate = activatePlayer "luigi"
    , objectOnActivate = \_ _ -> id
    , objectMass = 0
    , objectAcceleration = zeroVector
    , objectAffectedByGravity = True
    }
  , playerControls = marioControls1
  , playerControlVector = zeroVector
  }

player2InitialState :: Player
player2InitialState = Player
  { playerObject = Object
    { objectName = "luigi"
    , objectPosition = Position (level1TileSize * 4, level1TileSize * 8)
    , objectCollisionBoxes = [(Position (level1TileSize / 5 * 4, 0), Position (level1TileSize / 5 * 6, level1TileSize / 5 * 8))]
    , objectAppearance = Appearance
      { appearanceBox = (Position (level1TileSize / 5 * 4, 0), Position (level1TileSize / 5 * 6, level1TileSize / 5 * 8))
      , appearanceActualSize = fst luigiTexture
      , appearancePicture = snd luigiTexture
      }
    , objectVelocity = Vector (0, 0)
    , objectOnUpdate = \_ -> id
    , objectOnActivate = resizeSelf
    , objectMass = 0
    , objectAcceleration = zeroVector
    , objectAffectedByGravity = True
    }
  , playerControls =
      [ bindAction (Char 'd') (movePlayer (Vector (level1TileSize * 4, 0))) (movePlayer (Vector (-level1TileSize * 4, 0)))
      , bindAction (Char 'a') (movePlayer (Vector (-level1TileSize * 4, 0))) (movePlayer (Vector (level1TileSize * 4, 0)))
      , bindAction (Char 'w') (jumpPlayer (Vector (0, level1TileSize * 10))) (zeroAction)
      , bindAction (Char 'e') (activateObject True) (zeroAction)
      , bindAction (Char 'q') (activateObject False) (zeroAction)
      ]
  , playerControlVector = zeroVector
  }

-- | Advances game state one step further.
advanceGame :: Float -- ^ period of time (in seconds) needing to be advanced
            -> Game
            -> Game
advanceGame time = updateCamera . updatePhysics time . updateObjects


activatePlayer :: String -> Object -> Game -> Game
activatePlayer name object game =
  foldr (\player -> (objectOnActivate . playerObject $ player)
                    (objectsCollide object (playerObject player))
                    (playerObject player)
        )
        game
        (filter isTarget (gamePlayers game))
  where isTarget player = (objectName . playerObject $ player) == name


resizeSelf :: Bool -> Object -> Game -> Game
resizeSelf state self game = game
  { gamePlayers = map (\player -> if isSelf player
                                  then if state
                                       then enlarge player
                                       else reduce player
                                  else player
                      ) (gamePlayers game)
  }
  where isSelf player = (objectName . playerObject $ player) == (objectName self)
        enlarge = changeSize (Position (-level1TileSize / 5 * 3, -level1TileSize / 5 * 4), Position (level1TileSize / 5 * 9, level1TileSize / 5 * 12))
        reduce = changeSize (Position (level1TileSize / 5 * 4, 0), Position (level1TileSize / 5 * 6, level1TileSize / 5 * 8))
        changeSize rect player = player
          { playerObject = (playerObject player)
            { objectAppearance = (objectAppearance . playerObject $ player)
              { appearanceBox = rect
              }
            , objectCollisionBoxes = [rect]
            }
          }
