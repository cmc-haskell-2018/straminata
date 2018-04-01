module Model.Objects where

import Model.CommonTypes
import Visual.TextureLoader


defaultObject :: Object
defaultObject = Object
  { objectName = ""
  , objectPosition = Position (0, 0)
  , objectCollisionBoxes = []
  , objectAppearance = Appearance
    { appearanceBox = (Position (0, 0), Position (0, 0))
    , appearanceActualSize = fst transparentTexture
    , appearancePicture = snd transparentTexture
    }
  , objectVelocity = Vector (0, 0)
  , objectOnUpdate = \_ -> id
  , objectOnActivate = \_ _ -> id
  , objectMass = 0
  , objectAcceleration = zeroVector
  , objectAffectedByGravity = False
  }


buttonObject :: Object
buttonObject = defaultObject
  { objectCollisionBoxes = [(Position (0, 0), Position (48, 10))]
  , objectAppearance = Appearance
    { appearanceBox = (Position (0, 0), Position (48, 10))
    , appearanceActualSize = fst buttonTexture
    , appearancePicture = snd buttonTexture
    }
  , objectAffectedByGravity = False
  }

objectOnActivate :: Bool -> Object -> Game -> Game

doorObject :: Object
doorObject = defaultObject
  { objectCollisionBoxes = [(Position (0, 0), Position (32, 128))]
  , objectAppearance = Appearance
    { appearanceBox = (Position (0, 0), Position (32, 128))
    , appearanceActualSize = fst buttonTexture
    , appearancePicture = snd buttonTexture
    }
  , objectAffectedByGravity = False
  , objectOnActivate =
  }

  openDoorFn :: Bool -> Object -> Game -> Game
  openDoorFn state self game = game
    { gamePlayers = map (\player -> if isSelf player
                                    then if state
                                         then enlarge player
                                         else reduce player
                                    else player
                        ) (gamePlayers game)
    }
    where isSelf player = (objectName . playerObject $ player) == (objectName self)
          enlarge = changeSize (Position (-30, -40), Position (90, 120))
          reduce = changeSize (Position (40, 0), Position (60, 80))
          changeSize rect player = player
            { playerObject = (playerObject player)
              { objectAppearance = (objectAppearance . playerObject $ player)
                { appearanceBox = rect
                }
              , objectCollisionBoxes = [rect]
              }
            }


bindButtonAndDoor :: String -> Object -> Object -> [Object]
bindButtonAndDoor identifier button door =
  let buttonName = identifier ++ "_button"
      doorName = identifier ++ "_door"
  in [ button { objectName = buttonName
              , objectOnActivate = openDoorFn doorName
              }
     , door { objectName = doorName
            }
     ]