module Model.Objects where

import Model.CommonTypes
import Visual.TextureLoader
import Util.Common


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


finishButton :: Object
finishButton = defaultObject
  { objectCollisionBoxes = [(Position (0, 0), Position (50, 10))]
  , objectAppearance = Appearance
    { appearanceBox = (Position (0, 0), Position (50, 10))
    , appearanceActualSize = fst doorOpenTexture
    , appearancePicture = snd doorOpenTexture
    }
  , objectAffectedByGravity = False
  , objectOnActivate = quit
  }

quit :: Bool -> Object -> Game -> Game
quit = undefined


buttonObject :: Object
buttonObject = defaultObject
  { objectCollisionBoxes = [(Position (0, 0), Position (50, 10))]
  , objectAppearance = Appearance
    { appearanceBox = (Position (0, 0), Position (50, 10))
    , appearanceActualSize = fst buttonTexture
    , appearancePicture = snd buttonTexture
    }
  , objectAffectedByGravity = False
  }


doorObject :: Object
doorObject = defaultObject
  { objectCollisionBoxes = [(Position (0, -50), Position (50, 150))]
  , objectAppearance = Appearance
    { appearanceBox = (Position (0, -50), Position (50, 150))
    , appearanceActualSize = fst doorCloseTexture
    , appearancePicture = snd doorCloseTexture
    }
  , objectAffectedByGravity = False
  }


openDoorFn :: String -> Bool -> Object -> Game -> Game
openDoorFn doorName state self game =
  game { gameLevel = level
           { levelObjects =
               map (\object ->
                     if objectName object == objectName self
                     then if state
                          then changeTexture buttonPressTexture object
                          else changeTexture buttonTexture object
                     else if objectName object == doorName
                          then if state
                               then changeTexture doorOpenTexture object
                               else changeTexture doorCloseTexture object
                          else object
                   ) objects
           }
       }
  where level = gameLevel game
        objects = levelObjects level


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