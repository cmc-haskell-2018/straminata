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
  [ (\o game -> activatePlayer "ine" o $ activateCoin o game, \_ _ _ -> id)
  , (activateCoin, resizeSelf)
  ]

abilitiesWithoutResize :: PlayerAbilities
abilitiesWithoutResize =
  [ (activateCoin, \_ _ _ -> id)
  , (activateCoin, \_ _ _ -> id)
  ]


level1 :: Level
level1 = generateLevel
         (maps !! 0)
         (allObjects !! 0)
         backgroundTexture
         [ Position (level1TileSize * 2, level1TileSize * 5), Position (level1TileSize * 4, level1TileSize * 4)]
         abilitiesWithResize

level2 :: Level
level2 = generateLevel
         (maps !! 1)
         (allObjects !! 1)
         backgroundTexture
         [ Position (level1TileSize * 9, level1TileSize * 11), Position (level1TileSize * 10, level1TileSize * 11)]
         abilitiesWithoutResize

level3 :: Level
level3 = generateLevel
         (maps !! 2)
         (allObjects !! 2)
         backgroundTexture
         [ Position (level1TileSize * 2, level1TileSize * 2), Position (level1TileSize * 4, level1TileSize * 2)]
         abilitiesWithResize

level4 :: Level
level4 = generateLevel
         (maps !! 3)
         (allObjects !! 3)
         backgroundTexture
         [ Position (level1TileSize * 2, level1TileSize * 2), Position (level1TileSize * 28, level1TileSize * 2)]
         abilitiesWithResize

generateLevel :: Map -> [Object] -> Texture -> [Position] -> PlayerAbilities -> Level
generateLevel levelMap' objects background initialPositions abilities
  = Level
      { levelMap = levelMap'
      , levelColNumber = length levelMap'
      , levelRowNumber = length (head levelMap')
      , levelTileSize = level1TileSize
      , levelObjects = objects
      , levelBackground = Appearance
        { appearanceBox = (Position (0, 0), Position . (join (***)) (fromIntegral) $ initialWindowDimensions)
        , appearanceAnimation = [snd background]
        , appearanceActualSize = fst background
        }
      , levelCoinNumber = length $ filter (\o -> "coin" `isInfixOf` objectName o) objects
      , levelPlayersOut = []
      , levelStartPositions = initialPositions
      , levelPlayerAbilities = abilities
      }

generateLevelForPattern :: Int -> Level
generateLevelForPattern number = generateLevel
                                 (maps !! number)
                                 (allObjects !! number)
                                 backgroundTexture
                                 (allPositions !! number)
                                 (abilitiesList !! number)

levels :: [Level]
levels = map generateLevelForPattern [0..length patterns - 1]

generateObjectsForPattern :: Int -> [Object]
generateObjectsForPattern number = generateObjects nextLevel level1TileSize $ reverse (patterns !! number)
  where nextLevel = if number + 1 >= length levels then head levels else levels !! (number + 1)

allObjects :: [[Object]]
allObjects = map generateObjectsForPattern [0..length patterns - 1]

allPositions :: [[Position]]
allPositions =
  [ [ Position (level1TileSize * 2, level1TileSize * 5), Position (level1TileSize * 4, level1TileSize * 4)]
  , [ Position (level1TileSize * 9, level1TileSize * 11), Position (level1TileSize * 10, level1TileSize * 11)]
  , [ Position (level1TileSize * 2, level1TileSize * 2), Position (level1TileSize * 4, level1TileSize * 2)]
  , [ Position (level1TileSize * 2, level1TileSize * 2), Position (level1TileSize * 28, level1TileSize * 2)]
  ]

abilitiesList :: [PlayerAbilities]
abilitiesList =
  [ abilitiesWithResize
  , abilitiesWithoutResize
  , abilitiesWithResize
  , abilitiesWithResize
  ]


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
                "1" -> "exa"
                "2" -> "ine"
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
  , objectCollisionBoxes = [(Position (level1TileSize / 16 * 5, level1TileSize / 4), Position (level1TileSize / 16 * 11, level1TileSize / 4 * 3))]
  , objectAppearance = Appearance
      { appearanceBox = (Position (level1TileSize / 16 * 5, level1TileSize / 4), Position (level1TileSize / 16 * 11, level1TileSize / 4 * 3))
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


exaControls1 :: PlayerControls
exaControls1 =
  [ bindAction (SpecialKey KeyRight) (movePlayer (Vector (level1TileSize * 4, 0)) exaWalkAnimation) (movePlayer (Vector (-level1TileSize * 4, 0)) exaIdleAnimation)
  , bindAction (SpecialKey KeyLeft) (movePlayer (Vector (-level1TileSize * 4, 0)) exaWalkAnimation) (movePlayer (Vector (level1TileSize * 4, 0)) exaIdleAnimation)
  , bindAction (SpecialKey KeyUp) (jumpPlayer (Vector (0, level1TileSize * 5)) exaJumpAnimation) (idlePlayer exaIdleAnimation)
  , bindAction (Char 'c') (setAffectionByGravity False) (switchControlsAction exaControls2)
  , bindAction (SpecialKey KeyEnter) (activateObject True) (zeroAction)
  , bindAction (Char '/') (activateObject False) (zeroAction)
  ]

exaControls2 :: PlayerControls
exaControls2 =
  [ bindAction (SpecialKey KeyRight) (flightPlayer (Vector (level1TileSize * 10, 0))) (stopFlightPlayer (Vector (level1TileSize * 10, 0)))
  , bindAction (SpecialKey KeyLeft) (flightPlayer (Vector (-level1TileSize * 10, 0))) (stopFlightPlayer (Vector (-level1TileSize * 10, 0)))
  , bindAction (SpecialKey KeyUp) (flightPlayer (Vector (0, level1TileSize * 10))) (stopFlightPlayer (Vector (0, level1TileSize * 10)))
  , bindAction (SpecialKey KeyDown) (flightPlayer (Vector (0, -level1TileSize * 10))) (stopFlightPlayer (Vector (0, -level1TileSize * 10)))
  , bindAction (SpecialKey KeySpace) (stopPlayer) (zeroAction)
  , bindAction (Char 'c') (setAffectionByGravity True) (switchControlsAction exaControls1)
  , bindAction (SpecialKey KeyEnter) (activateObject True) (zeroAction)
  , bindAction (Char '/') (activateObject False) (zeroAction)
  ]


playerInitialState :: Player
playerInitialState = Player
  { playerObject = Object
    { objectName = "exa"
    , objectPosition = Position (level1TileSize * 2, level1TileSize * 4)
    , objectCollisionBoxes = [(Position (0, 0), Position (level1TileSize / 4 * 3, level1TileSize / 4 * 3))]
    , objectAppearance = Appearance
      { appearanceBox = (Position (0, 0), Position (level1TileSize / 4 * 3, level1TileSize / 4 * 3))
      , appearanceActualSize = fst exaIdle1
      , appearanceAnimation = exaIdleAnimation
      }
    , objectVelocity = Vector (0, 0)
    , objectOnUpdate = \o game -> activatePlayer "ine" o $ activateCoin o game
    , objectOnActivate = \_ _ _ -> id
    , objectMass = 0
    , objectAcceleration = zeroVector
    , objectAffectedByGravity = True
    }
  , playerControls = exaControls1
  , playerControlVector = zeroVector
  , playerCoins = 0
  }

player2InitialState :: Player
player2InitialState = Player
  { playerObject = Object
    { objectName = "ine"
    , objectPosition = Position (level1TileSize * 4, level1TileSize * 6)
    , objectCollisionBoxes = [(Position (0, 0), Position (level1TileSize / 4 * 3, level1TileSize / 4 * 3))]
    , objectAppearance = Appearance
      { appearanceBox = (Position (0, 0), Position (level1TileSize / 4 * 3, level1TileSize / 4 * 3))
      , appearanceActualSize = fst ineIdle1
      , appearanceAnimation = ineIdleAnimation
      }
    , objectVelocity = Vector (0, 0)
    , objectOnUpdate = activateCoin
    , objectOnActivate = resizeSelf
    , objectMass = 0
    , objectAcceleration = zeroVector
    , objectAffectedByGravity = True
    }
  , playerControls =
      [ bindAction (Char 'd') (movePlayer (Vector (level1TileSize * 4, 0)) ineWalkAnimation) (movePlayer (Vector (-level1TileSize * 4, 0)) ineIdleAnimation)
      , bindAction (Char 'a') (movePlayer (Vector (-level1TileSize * 4, 0)) ineWalkAnimation) (movePlayer (Vector (level1TileSize * 4, 0)) ineIdleAnimation)
      , bindAction (Char 'w') (jumpPlayer (Vector (0, level1TileSize * 5)) ineJumpAnimation) (idlePlayer ineIdleAnimation)
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
        enlarge = changeSize (Position (-level1TileSize / 5 * 4, -level1TileSize / 5 * 4), Position (level1TileSize / 5 * 10, level1TileSize / 5 * 10))
        reduce = changeSize (Position (0, 0), Position (level1TileSize / 4 * 3, level1TileSize / 4 * 3))
        changeSize rect player = player
          { playerObject = (playerObject player)
            { objectAppearance = (objectAppearance . playerObject $ player)
              { appearanceBox = rect
              }
            , objectCollisionBoxes = [rect]
            }
          }

