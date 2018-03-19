{-# OPTIONS_GHC -Wall #-}
-- | __Straminata__ is a puzzle platform game written completely in Haskell.
module Straminata where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game hiding (Vector)

import Model.CommonTypes
import Model.Map
import Visual.WindowConstants
import Visual.Renderer
import Visual.TextureLoader
import Util.Common
import Util.Controls

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

-- todo: get from file
playerInitialState :: Player
playerInitialState = Player
  { playerObject = Object
    { objectName = "mario"
    , objectPosition = Position (0, 0)
    , objectCollisionBoxes = [(Position (0, 0), Position (60, 80))]
    , objectAppearance = Appearance
      { appearanceBox = (Position (0, 0), Position (60, 80))
      , appearanceActualSize = fst marioTexture
      , appearancePicture = snd marioTexture
      }
    , objectVelocity = Vector (0, 0)
    , objectOnUpdate = activatePlayer "luigi"
    , objectOnActivate = \_ _ -> id
    , objectMass = 0
    , objectAcceleration = zeroVector
    }
  , playerControls =
      [ bindAction (SpecialKey KeyRight) (movePlayer (Vector (10, 0))) (movePlayer (Vector (-10, 0)))
      , bindAction (SpecialKey KeyLeft) (movePlayer (Vector (-10, 0))) (movePlayer (Vector (10, 0)))
      , bindAction (SpecialKey KeyUp) (movePlayer (Vector (0, 10))) (movePlayer (Vector (0, -10)))
      , bindAction (SpecialKey KeyDown) (movePlayer (Vector (0, -10))) (movePlayer (Vector (0, 10)))
      ]
  , playerControlVector = zeroVector
  }

player2InitialState :: Player
player2InitialState = Player
  { playerObject = Object
    { objectName = "luigi"
    , objectPosition = Position (0, 0)
    , objectCollisionBoxes = [(Position (0, 0), Position (60, 80))]
    , objectAppearance = Appearance
      { appearanceBox = (Position (0, 0), Position (60, 80))
      , appearanceActualSize = fst luigiTexture
      , appearancePicture = snd luigiTexture
      }
    , objectVelocity = Vector (0, 0)
    , objectOnUpdate = \_ -> id
    , objectOnActivate = resizeSelf
    , objectMass = 0
    , objectAcceleration = zeroVector
    }
  , playerControls =
    [ bindAction (Char 'd') (movePlayer (Vector (20, 0))) (movePlayer (Vector (-20, 0)))
    , bindAction (Char 'a') (movePlayer (Vector (-20, 0))) (movePlayer (Vector (20, 0)))
    , bindAction (Char 'w') (movePlayer (Vector (0, 20))) (movePlayer (Vector (0, -20)))
    , bindAction (Char 's') (movePlayer (Vector (0, -20))) (movePlayer (Vector (0, 20)))
    , bindAction (Char 'e') (setTextureByName "luigi" marioTexture) (setTextureByName "luigi" luigiTexture)
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
        enlarge = changeSize (Position (-30, -40), Position (90, 120))
        reduce = changeSize (Position (0, 0), Position (60, 80))
        changeSize rect player = player
          { playerObject = (playerObject player)
            { objectAppearance = (objectAppearance . playerObject $ player)
              { appearanceBox = rect
              }
            , objectCollisionBoxes = [rect]
            }
          }


-- temporary
-- updatePlayers :: Game -> Game
-- updatePlayers game = let playerList = gamePlayers game
--                          player1 = (playerList !! 0)
--                          player2 = (playerList !! 1)
--                          object1 = playerObject player1
--                          object2 = playerObject player2
--                          playersColliding = objectsCollide object1 object2
--                          recolorPlayer player = player
--                                                 { playerObject = (playerObject player)
--                                                   { objectAppearance = (objectAppearance . playerObject $ player)
--                                                     { appearancePicture =
--                                                       (if playersColliding
--                                                       then Color red
--                                                       else Color blue)
--                                                         (Polygon $ formPath
--                                                           (appearanceBox . objectAppearance . playerObject $ player)
--                                                         )
--                                                     }
--                                                   }
--                                                 }
--                      in game { gamePlayers = map recolorPlayer playerList }
