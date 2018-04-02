module Model.Objects where

import Graphics.Gloss.Interface.IO.Game hiding (Vector, line)

import Control.Monad (join)
import Control.Arrow ((***))
import Data.Maybe(isJust, fromJust)
import Data.List(elemIndex, isInfixOf, isSuffixOf, union, (\\))
import Data.Tuple(swap)

import Model.CommonTypes
import Model.LevelPatterns
import Util.Controls
import Util.Common
import Util.Constants
import Visual.TextureLoader
import Visual.WindowConstants


-- Level abilities
abilitiesWithResize :: PlayerAbilities
abilitiesWithResize =
  [ (\o game -> activatePlayer "luigi" o $ activateCoin o game, \_ _ _ -> id)
  , (activateCoin, resizeSelf)
  ]

abilitiesWithoutResize :: PlayerAbilities
abilitiesWithoutResize =
  [ (activateCoin, \_ _ _ -> id)
  , (activateCoin, \_ _ _ -> id)
  ]

-- Levels

level1 :: Level
level1 = Level
  { levelMap = level1Map
  , levelColNumber = length level1Map
  , levelRowNumber = length (head level1Map)
  , levelTileSize = level1TileSize
  , levelObjects = objects1
  , levelBackground = Appearance
    { appearanceBox = (Position (0, 0), Position . (join (***)) (fromIntegral) $ initialWindowDimensions)
    , appearanceAnimation = [snd backgroundTexture]
    , appearanceActualSize = fst backgroundTexture
    }
  , levelCoinNumber = length coins1
  , levelPlayersOut = []
  , levelStartPositions = [ Position (level1TileSize * 2, level1TileSize * 6)
                          , Position (level1TileSize * 4, level1TileSize * 6)]
  , levelPlayerAbilities = abilitiesWithResize
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
    , appearanceAnimation = [snd backgroundTexture]
    , appearanceActualSize = fst backgroundTexture
    }
  , levelCoinNumber = length coins2
  , levelPlayersOut = []
  , levelStartPositions = [ Position (level1TileSize * 9, level1TileSize * 11)
                          , Position (level1TileSize * 10, level1TileSize * 11)]
  , levelPlayerAbilities = abilitiesWithoutResize
  }

objects2 :: [Object]
objects2 = generateObjects level3 level1TileSize $ reverse level2Pattern

coins2 :: [Object]
coins2 = filter (\o -> "coin" `isInfixOf` objectName o) objects2


level3 :: Level
level3 = Level
  { levelMap = level3Map
  , levelColNumber = length level3Map
  , levelRowNumber = length (head level3Map)
  , levelTileSize = level1TileSize
  , levelObjects = objects3
  , levelBackground = Appearance
    { appearanceBox = (Position (0, 0), Position . (join (***)) (fromIntegral) $ initialWindowDimensions)
    , appearanceAnimation = [snd backgroundTexture]
    , appearanceActualSize = fst backgroundTexture
    }
  , levelCoinNumber = length coins3
  , levelPlayersOut = []
  , levelStartPositions = [ Position (level1TileSize * 2, level1TileSize * 8)
                          , Position (level1TileSize * 4, level1TileSize * 8)]
  , levelPlayerAbilities = abilitiesWithoutResize
  }

objects3 :: [Object]
objects3 = generateObjects level1 level1TileSize $ reverse level3Pattern

coins3 :: [Object]
coins3 = filter (\o -> "coin" `isInfixOf` objectName o) objects3

-- /Levels

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
        transferSymbol y (x, 'q' : n) = Just $ [finishButton
            { objectPosition = Position (x * size, y * size)
            , objectOnActivate = changeLevel nextLevel
            , objectName = "finish_" ++ case n of
                "1" -> "mario"
                "2" -> "luigi"
                _ -> undefined
            , objectAppearance =
                let tex = case n of
                          "1" -> teleport1Texture
                          "2" -> teleport2Texture
                          _ -> undefined
                in Appearance
                { appearanceBox = (Position (0, 0), Position (level1TileSize, level1TileSize / 5))
                , appearanceActualSize = fst tex
                , appearanceAnimation = [snd tex]
                }
            }
          ]
        transferSymbol y (x, 'c' : _) = Just $
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
    , appearanceAnimation = [snd transparentTexture]
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
  { objectCollisionBoxes = [(Position (0, 0), Position (level1TileSize, level1TileSize / 5))]
  }

changeLevel :: Level -> Bool -> Player -> Object -> Game -> Game
changeLevel next True player object game =
  if (name player) `isSuffixOf` (objectName object)
  then let playersOut = (levelPlayersOut . gameLevel $ game) `union` [player]
       in if null (playersOut \\ players) && null (players \\ playersOut)
          then Game
            {
              gamePlayers = map
                (\(index, player') -> player'
                    { playerCoins = (playerCoins player')
                    , playerObject = (playerObject player')
                        { objectPosition = positions !! index
                        , objectOnUpdate =  fst (abilities !! index)
                        , objectOnActivate =  snd (abilities !! index)
                        }
                    }
                )
                (zip [0..] players)
            , gameLevel = next
            , gameCamera = Camera
               { cameraPosition = Position (0, 0)
               , cameraRatio = 1
               }
            }
          else game {gameLevel = (gameLevel game) {levelPlayersOut = playersOut}} -- todo: remove exited player
  else game
  where players = gamePlayers game
        abilities = levelPlayerAbilities next
        positions = levelStartPositions next
        name = objectName . playerObject
changeLevel _ False _ _ game = game

coinObject :: Object
coinObject = defaultObject
  { objectName = "coin"
  , objectCollisionBoxes = [(Position (0, 0), Position (level1TileSize, level1TileSize))]
  , objectAppearance = Appearance
      { appearanceBox = (Position (0, 0), Position (level1TileSize, level1TileSize))
      , appearanceActualSize = fst coinTexture
      , appearanceAnimation = [snd coinTexture]
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
    , appearanceAnimation = [snd buttonTexture]
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
    , appearanceAnimation = [snd doorCloseTexture]
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
  { gamePlayers = [ playerInitialState {playerObject = (playerObject playerInitialState) {objectPosition = position1}}
                  , player2InitialState {playerObject = (playerObject player2InitialState) {objectPosition = position2}}]
  , gameLevel = level1
  , gameCamera = Camera
    { cameraPosition = Position (0, 0)
    , cameraRatio = 1
    }
  }
  where positions = levelStartPositions level1
        position1 = positions !! 0
        position2 = positions !! 1


marioControls1 :: PlayerControls
marioControls1 =
  [ bindAction (SpecialKey KeyRight) (movePlayer (Vector (level1TileSize * 4, 0))) (movePlayer (Vector (-level1TileSize * 4, 0)))
  , bindAction (SpecialKey KeyLeft) (movePlayer (Vector (-level1TileSize * 4, 0))) (movePlayer (Vector (level1TileSize * 4, 0)))
  , bindAction (SpecialKey KeyUp) (jumpPlayer (Vector (0, level1TileSize * 5)) upAnimation) (zeroAction)
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
    { objectName = "exa"
    , objectPosition = Position (level1TileSize * 2, level1TileSize * 8)
    , objectCollisionBoxes = [(Position (level1TileSize / 5 * 4, 0), Position (level1TileSize / 5 * 6, level1TileSize / 5 * 6))]
    , objectAppearance = Appearance
      { appearanceBox = (Position (level1TileSize / 5 * 4, 0), Position (level1TileSize / 5 * 6, level1TileSize / 5 * 5))
      , appearanceActualSize = fst marioTexture
      , appearanceAnimation = [snd marioTexture]
      }
    , objectVelocity = Vector (0, 0)
    , objectOnUpdate = \o game -> activatePlayer "luigi" o $ activateCoin o game
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
      , appearanceActualSize = fst luigiTexture1
      , appearanceAnimation = luigiStandingAnimation
      }
    , objectVelocity = Vector (0, 0)
    , objectOnUpdate = activateCoin
    , objectOnActivate = resizeSelf
    , objectMass = 0
    , objectAcceleration = zeroVector
    , objectAffectedByGravity = True
    }
  , playerControls =
      [ bindAction (Char 'd') (movePlayer (Vector (level1TileSize * 4, 0))) (movePlayer (Vector (-level1TileSize * 4, 0)))
      , bindAction (Char 'a') (movePlayer (Vector (-level1TileSize * 4, 0))) (movePlayer (Vector (level1TileSize * 4, 0)))
      , bindAction (Char 'w') (jumpPlayer (Vector (0, level1TileSize * 5)) upAnimation) (zeroAction)
      , bindAction (Char 'e') (activateObject True) (zeroAction)
      , bindAction (Char 'q') (activateObject False) (zeroAction)
      , bindAction (Char ']') (resetAction) (zeroAction)
      ]
  , playerControlVector = zeroVector
  , playerCoins = 0
  }

resetAction :: Action
resetAction =
  GameAction (\_ game ->
    let players = gamePlayers game
        player1 = players !! 0
        player2 = players !! 1
        positions = levelStartPositions (gameLevel game)
        position1 = positions !! 0
        position2 = positions !! 1
    in Game { gamePlayers = [ playerInitialState { playerCoins = playerCoins player1
                                         , playerObject = (playerObject player1) {objectPosition = position1}
                                         }
                    , player2InitialState { playerCoins = playerCoins player2
                                          , playerObject = (playerObject player2) {objectPosition = position2}
                                          }
                    ]
    , gameLevel = (gameLevel game) {levelPlayersOut = []}
    , gameCamera = Camera
        { cameraPosition = Position (0, 0)
        , cameraRatio = 1
        }
    })

--activateCoin :: Object -> Game -> Game
--activateCoin self game =
--  foldr (\object acc ->
--          if objectsCollide self object
--          then takeCoin ((appearanceAnimation . objectAppearance $ object) /= snd transparentTexture) player object acc
--          else acc
--        )
--        game
--        (filter isCoin (levelObjects . gameLevel $ game))
--  where isCoin o = "coin" `isInfixOf` (objectName o)
--        player = head $ filter (\p -> name p == objectName self) (gamePlayers game)
--        name = objectName . playerObject

activateCoin ::  Object -> Game -> Game
activateCoin object game =
  foldr (\coin acc ->
                   takeCoin (((appearanceAnimation . objectAppearance $ coin) /= [snd transparentTexture])
                             && (objectsCollide (playerObject player) coin))
                             player coin acc
        )
        game coins
  where player = head . filter isTarget $ gamePlayers game
        isTarget player' = (playerObject $ player') == object
        coins = filter (\coin -> isInfixOf "coin" (objectName coin)) (levelObjects . gameLevel $ game)

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

