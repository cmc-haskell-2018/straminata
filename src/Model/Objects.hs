module Model.Objects where

import Graphics.Gloss.Interface.IO.Game hiding (Vector, line)

import Control.Monad (join)
import Control.Arrow ((***))
import Data.Maybe(isJust, fromJust)
import Data.List(elemIndex, isInfixOf)
import Data.Tuple(swap)

import Model.CommonTypes
import Model.LevelPatterns
import Util.Controls
import Util.Common
import Util.Constants
import Visual.TextureLoader
import Visual.WindowConstants


level1 :: Level
level1 = Level
  { levelMap = level1Map
  , levelColNumber = length level1Map
  , levelRowNumber = length (head level1Map)
  , levelTileSize = level1TileSize
  , levelObjects = objects1
  , levelBackground = Appearance
    { appearanceBox = (Position (0, 0), Position . (join (***)) (fromIntegral) $ initialWindowDimensions)
    , appearancePicture = snd backgroundTexture
    , appearanceActualSize = fst backgroundTexture
    }
  , levelCoinNumber = length coins1
  }

objects1 :: [Object]
objects1 = generateObjects level2 level1TileSize $ reverse level1Pattern

coins1 :: [Object]
coins1 = filter (\o -> "coin" `isInfixOf` objectName o) objects1

level2 :: Level
level2 = Level
  { levelMap = level2Map
  , levelColNumber = length level2Map
  , levelRowNumber = length (head level2Map)
  , levelTileSize = level1TileSize
  , levelObjects = objects2
  , levelBackground = Appearance
    { appearanceBox = (Position (0, 0), Position . (join (***)) (fromIntegral) $ initialWindowDimensions)
    , appearancePicture = snd backgroundTexture
    , appearanceActualSize = fst backgroundTexture
    }
  , levelCoinNumber = length coins2
  }

objects2 :: [Object]
objects2 = generateObjects level1 level1TileSize $ reverse level2Pattern

coins2 :: [Object]
coins2 = filter (\o -> "coin" `isInfixOf` objectName o) objects2

generateObjects :: Level -> Float -> [String] -> [Object]
generateObjects nextLevel size pattern = foldr (\t acc -> acc ++ transferLine t) [] $ zip [1..] pattern
  where transferLine (y, line) = foldr (
          \x acc -> let t = transferSymbol y x in if isJust t then acc ++ fromJust t else acc)
          [] $ zip [1..] (words line)
        transferSymbol y (x, 'b' : n) =
          let coord = tableIndex ("d" ++ n) pattern
          in Just
            $ bindButtonAndDoor n
                                (buttonObject {objectPosition = Position (x * size, y * size)})
                                (doorObject {objectPosition = Position (fromIntegral (fst coord) * size, fromIntegral (snd coord) * size)})
        transferSymbol y (x, "q") = Just $ [finishButton {objectPosition = Position (x * size, y * size), objectOnActivate = changeLevel nextLevel}]
        transferSymbol y (x, "c") = Just $
          [ coinObject
            { objectPosition = Position (x * size, y * size)
            , objectName = "coin_" ++ show y ++ "_" ++ show x
            }
          ]
        transferSymbol _ _ = Nothing


tableIndex :: String -> [String] -> (Int, Int)
tableIndex e es = swap . withIndex $ filter (\xs -> any (== e) . words $ snd xs) $ zip [1 :: Int ..] es
  where withIndex [(index, xs)] = (index, (fromJust $ elemIndex e $ words xs) + 1)
        withIndex _ = undefined



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
  , objectOnActivate = \_ _ _ -> id
  , objectMass = 0
  , objectAcceleration = zeroVector
  , objectAffectedByGravity = False
  }


finishButton :: Object
finishButton = defaultObject
  { objectName = "finish"
  , objectCollisionBoxes = [(Position (0, 0), Position (level1TileSize, level1TileSize / 5))]
  , objectAppearance = Appearance
    { appearanceBox = (Position (0, 0), Position (level1TileSize, level1TileSize / 5))
    , appearanceActualSize = fst doorOpenTexture
    , appearancePicture = snd doorOpenTexture
    }
  , objectOnActivate = changeLevel level1
  }

changeLevel :: Level -> Bool -> Player -> Object -> Game -> Game
changeLevel next True _ _ _ = Game
  { gamePlayers = [playerInitialState, player2InitialState]
  , gameLevel = next
  , gameCamera = Camera
    { cameraPosition = Position (0, 0)
    , cameraRatio = 1
    }
  }
changeLevel _ False _ _ game = game

coinObject :: Object
coinObject = defaultObject
  { objectName = "coin"
  , objectCollisionBoxes = [(Position (0, 0), Position (level1TileSize, level1TileSize))]
  , objectAppearance = Appearance
      { appearanceBox = (Position (0, 0), Position (level1TileSize, level1TileSize))
      , appearanceActualSize = fst coinTexture
      , appearancePicture = snd coinTexture
      }
  , objectOnActivate = takeCoin
  }

takeCoin :: Bool -> Player -> Object -> Game -> Game
takeCoin False _ _ game = game
takeCoin True player self game = game
  { gameLevel = level
    { levelObjects = map (\o -> if objectName self == objectName o
                                then changeTexture transparentTexture (o {objectOnActivate = \_ _ _ -> id})
                                else o
                         ) $ objects
    }
  , gamePlayers = map (\p -> if name player == name p
                             then p {playerCoins = (playerCoins p) + 1}
                             else p
                      ) (gamePlayers game)
  }
  where level = gameLevel game
        objects = levelObjects level
        name = objectName . playerObject


buttonObject :: Object
buttonObject = defaultObject
  { objectCollisionBoxes = [(Position (0, 0), Position (level1TileSize, level1TileSize / 5))]
  , objectAppearance = Appearance
    { appearanceBox = (Position (0, 0), Position (level1TileSize, level1TileSize / 5))
    , appearanceActualSize = fst buttonTexture
    , appearancePicture = snd buttonTexture
    }
  , objectAffectedByGravity = False
  }


closedDoorBox :: [(Position, Position)]
closedDoorBox = [(Position (0, 0), Position (level1TileSize, 2 * level1TileSize))]

openedDoorBox :: [(Position, Position)]
openedDoorBox = [(Position (0, -level1TileSize), Position (level1TileSize, 0)),
                 (Position (0, 2 * level1TileSize), Position (level1TileSize, 3 * level1TileSize))]

doorObject :: Object
doorObject = defaultObject
  { objectCollisionBoxes = closedDoorBox
  , objectAppearance = Appearance
    { appearanceBox = (Position (0, -level1TileSize), Position (level1TileSize, 3 * level1TileSize))
    , appearanceActualSize = fst doorCloseTexture
    , appearancePicture = snd doorCloseTexture
    }
  , objectAffectedByGravity = False
  }


openDoorFn :: String -> Bool -> Player -> Object -> Game -> Game
openDoorFn doorName state _ self game =
  game { gameLevel = level
           { levelObjects =
               map (\object ->
                     if objectName object == objectName self
                     then if state
                          then changeTexture buttonPressTexture object
                          else changeTexture buttonTexture object
                     else if objectName object == doorName
                          then if state
                               then changeTexture doorOpenTexture (object {objectCollisionBoxes = openedDoorBox})
                               else changeTexture doorCloseTexture (object {objectCollisionBoxes = closedDoorBox})
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



-- | Initial game state.
initialWorld :: Game
initialWorld = Game
  { gamePlayers = [playerInitialState, player2InitialState]
  , gameLevel = level1
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


playerInitialState :: Player
playerInitialState = Player
  { playerObject = Object
    { objectName = "mario"
    , objectPosition = Position (level1TileSize * 2, level1TileSize * 8)
    , objectCollisionBoxes = [(Position (level1TileSize / 5 * 4, 0), Position (level1TileSize / 5 * 6, level1TileSize / 5 * 6))]
    , objectAppearance = Appearance
      { appearanceBox = (Position (level1TileSize / 5 * 4, 0), Position (level1TileSize / 5 * 6, level1TileSize / 5 * 5))
      , appearanceActualSize = fst marioTexture
      , appearancePicture = snd marioTexture
      }
    , objectVelocity = Vector (0, 0)
    , objectOnUpdate = activatePlayer "luigi"
    , objectOnActivate = \_ _ _ -> id
    , objectMass = 0
    , objectAcceleration = zeroVector
    , objectAffectedByGravity = True
    }
  , playerControls = marioControls1
  , playerControlVector = zeroVector
  , playerCoins = 0
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
  , playerCoins = 0
  }

activatePlayer :: String -> Object -> Game -> Game
activatePlayer name object game =
  foldr (\player acc ->
                    objectOnActivate (playerObject player)
                    (objectsCollide object (playerObject player))
                    player
                    (playerObject player)
                    acc
        )
        game
        (filter isTarget (gamePlayers game))
  where isTarget player = (objectName . playerObject $ player) == name


resizeSelf :: Bool -> Player -> Object -> Game -> Game
resizeSelf state _ self game = game
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

