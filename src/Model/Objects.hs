module Model.Objects where

import Model.CommonTypes
import Visual.TextureLoader

buttonObject :: Object
buttonObject = Object
  { objectName = ""
  , objectPosition = Position (0, 0)
  , objectCollisionBoxes = [(Position (0, 0), Position (60, 80))]
  , objectAppearance = Appearance
    { appearanceBox = (Position (0, 0), Position (50, 50))
    , appearanceActualSize = fst luigiTexture
    , appearancePicture = snd luigiTexture
    }
  , objectVelocity = Vector (0, 0)
  , objectOnUpdate = \_ -> id
  , objectOnActivate = \_ _ -> id
  , objectMass = 0
  , objectAcceleration = zeroVector
  , objectAffectedByGravity = False
  }