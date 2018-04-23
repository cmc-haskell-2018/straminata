module Generator where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Control.Monad (join)
import Control.Arrow ((***))
import Data.Maybe(isJust, fromJust)
import System.IO.Unsafe(unsafePerformIO)

import Model.CommonTypes
import Visual.Renderer
import Visual.TextureLoader
import Visual.WindowConstants

-- | Map generator
data Generator = Generator
  { generatorButtons :: [[Button]]
  , generatorSubmitted :: Bool
  }

initialGeneratorState :: Generator
initialGeneratorState = Generator
  { generatorButtons = generateEmptyButtons
  , generatorSubmitted = False
  }

-- | Button that represents a map tile
data Button = Button
  { buttonPos :: Position
  , buttonState :: ButtonState
  , buttonNum :: Int
  }

-- | Map tile type
data ButtonState = None | Grass | Dirt | Coin | ButtonObject | Door | Exit1 | Exit2 | Player1 | Player2
  deriving (Eq, Enum, Bounded)

-- | Get next tile variant
nextState :: ButtonState -> ButtonState
nextState s = if s == (maxBound :: ButtonState) then minBound else succ s

-- | Get previous tile variant
prevState :: ButtonState -> ButtonState
prevState s = if s == (minBound :: ButtonState) then maxBound else pred s

-- | Get map tile appearance
buttonAppearance :: ButtonState -> Position -> Appearance
buttonAppearance None = createAppearance transparentTexture
buttonAppearance Grass = createAppearance dirtTextureWithGrass
buttonAppearance Dirt = createAppearance dirtTextureWithoutGrass
buttonAppearance Coin = createAppearance coinTexture
buttonAppearance ButtonObject = createAppearance buttonTexture
buttonAppearance Door = createAppearance doorCloseTexture
buttonAppearance Exit1 = createAppearance teleport1Texture
buttonAppearance Exit2 = createAppearance teleport2Texture
buttonAppearance Player1 = createAppearance exaIdle1
buttonAppearance Player2 = createAppearance ineIdle1

-- | Default appearance template
createAppearance :: Texture -> Position -> Appearance
createAppearance texture (Position (x, y)) = Appearance
  { appearanceBox = ( Position (x * buttonLinearSize, y * buttonLinearSize)
                    , Position ((x + 1) * buttonLinearSize, (y + 1) * buttonLinearSize))
  , appearanceActualSize = fst texture
  , appearanceAnimation = [snd texture]
  }

nextNum :: Int -> Int
nextNum 9 = 0
nextNum n = n + 1

prevNum :: Int -> Int
prevNum 0 = 9
prevNum n = n - 1

-- | UI button
data SystemButton = SystemButton
  { systemButtonPosition :: Position
  , systemButtonText :: String
  }

systemButtons :: [SystemButton]
systemButtons = generateSystemButtons

generateSystemButtons :: [SystemButton]
generateSystemButtons =
  [ SystemButton
      { systemButtonPosition = Position $ join (***) ((\x -> x - 200) . (/2) . fromIntegral) initialWindowDimensions
      , systemButtonText = "Generate"
      }
  ]



generateEmptyButtons :: [[Button]]
generateEmptyButtons = map (scaleRow buttonLinearSize) rowSetter
  where
    rowSetter = createWithNumber maximumMapSize (\r n -> map (setterY n) r) row
    row = createWithNumber maximumMapSize setterX $ Button { buttonPos = Position (0, 0), buttonState = None, buttonNum = 0 }
    setterX button x = button { buttonPos = setX x (buttonPos button) }
    setterY y button = button { buttonPos = setY y (buttonPos button) }
    setX x (Position (_, y)) = Position (x, y)
    setY y (Position (x, _)) = Position (x, y)
    scaleRow times = map (\b -> b { buttonPos = multPos times (buttonPos b) })
    multPos times (Position (x, y)) = Position (x * times, y * times)

createWithNumber :: Float -> (a -> Float -> a) -> a -> [a]
createWithNumber maxNum setter base = map (setter base) [-maxNum / 2 + 1 .. maxNum / 2]



maximumMapSize :: Float
maximumMapSize = 30

buttonSize :: Dimensions
buttonSize = Dimensions $ double . (/ maximumMapSize) . fromIntegral . (\x -> x - 100) . snd $ initialWindowDimensions
  where double x = (x, x)

buttonLinearSize :: Float
buttonLinearSize = fst . unwrap $ buttonSize



renderGenerator :: Generator -> Picture
renderGenerator generator = Pictures
                          $ map draw (concat . generatorButtons $ generator)
                          ++ map borders (concat . generatorButtons $ generator)
                          ++ map number ( filter (\b -> buttonNum b /= 0 && correctState b)
                                        . concat
                                        . generatorButtons
                                        $ generator)
                          ++ map system systemButtons
  where draw button = uncurry Translate (unwrap . buttonPos $ button)
                    $ appearanceToPicture $ buttonAppearance (buttonState button) (buttonPos button)
        borders button = Color cyan
                       $ uncurry Translate (unwrap . buttonPos $ button)
                       $ uncurry rectangleWire (unwrap $ buttonSize)
        number button = Color cyan
                      $ uncurry Translate (unwrap . buttonPos $ button)
                      $ Scale 0.1 0.1
                      $ Text (show $ buttonNum button)
        correctState button = let s = buttonState button
                              in s == Door
                              || s == ButtonObject
        system button = Pictures
          [ Color black
            $ uncurry Translate ((\(x, y) -> (x + 80, y)) . unwrap . systemButtonPosition $ button)
            $ uncurry rectangleWire (160, 50)
          , Color black
            $ uncurry Translate (unwrap . systemButtonPosition $ button)
            $ Scale 0.15 0.15
            $ Text (systemButtonText $ button)
          ]



handleGeneratorInput :: Event -> Generator -> Generator
handleGeneratorInput (EventKey (MouseButton LeftButton) Up (Modifiers {shift = Up, ctrl = Up, alt = Up}) (x, y)) generator
  = if clickedSubmit (x, y)
    then generator { generatorSubmitted = True }
    else generator
         { generatorButtons = map (map (clickLeftOnTileButton (x, y))) $ generatorButtons generator
         }
handleGeneratorInput (EventKey (MouseButton LeftButton) Up (Modifiers {shift = Down, ctrl = Up, alt = Up}) (x, y)) generator
  = generator
  { generatorButtons = map (map (clickLeftOnTileButtonWithShift (x, y))) $ generatorButtons generator
  }
handleGeneratorInput (EventKey (MouseButton RightButton) Up (Modifiers {shift = Up, ctrl = Up, alt = Up}) (x, y)) generator
  = generator
  { generatorButtons = map (map (clickRightOnTileButton (x, y))) $ generatorButtons generator
  }
handleGeneratorInput (EventKey (MouseButton RightButton) Up (Modifiers {shift = Down, ctrl = Up, alt = Up}) (x, y)) generator
  = generator
  { generatorButtons = map (map (clickRightOnTileButtonWithShift (x, y))) $ generatorButtons generator
  }
handleGeneratorInput _ generator = generator

clickedSubmit :: (Float, Float) -> Bool
clickedSubmit (x, y) = let (x', y') = unwrap . systemButtonPosition . head $ systemButtons
                       in x >= x'
                       && x <= x' + 160
                       && y >= y' - 25
                       && y <= y' + 25

clickLeftOnTileButton :: (Float, Float) -> Button -> Button
clickLeftOnTileButton (x, y) button = if hit (x, y) (unwrap . buttonPos $ button)
                                      then button { buttonState = nextState . buttonState $ button }
                                      else button

clickRightOnTileButton :: (Float, Float) -> Button -> Button
clickRightOnTileButton (x, y) button = if hit (x, y) (unwrap . buttonPos $ button)
                                      then button { buttonState = prevState . buttonState $ button }
                                      else button

clickLeftOnTileButtonWithShift :: (Float, Float) -> Button -> Button
clickLeftOnTileButtonWithShift (x, y) button = if hit (x, y) (unwrap . buttonPos $ button)
                                               then button { buttonNum = nextNum . buttonNum $ button }
                                               else button

clickRightOnTileButtonWithShift :: (Float, Float) -> Button -> Button
clickRightOnTileButtonWithShift (x, y) button = if hit (x, y) (unwrap . buttonPos $ button)
                                               then button { buttonNum = prevNum . buttonNum $ button }
                                               else button

hit :: (Float, Float) -> (Float, Float) -> Bool
hit (x, y) (x', y') = x >= x' - buttonLinearSize / 2
                   && x <= x' + buttonLinearSize / 2
                   && y >= y' - buttonLinearSize / 2
                   && y <= y' + buttonLinearSize / 2



advanceGenerator :: Float -> Generator -> Generator
advanceGenerator _ generator =
  if generatorSubmitted generator
  then let bs = generatorButtons generator
       in unsafePerformIO (writeFile "level/level.lvl" ("30\n"
                                                        ++ toString bs
                                                        ++ addBack bs
                                                        ++ addPlayers bs
                                                        ++ addAbilities bs)
                           >> return initialGeneratorState)
  else generator

toString :: [[Button]] -> String
toString = unlines . reverse . map toStringRow
  where toStringRow = unwords . map toStringElem
        toStringElem elem' = case buttonState elem' of
          None -> "t1"
          Grass -> "w1"
          Dirt -> "w2"
          Coin -> "c1"
          ButtonObject -> "b" ++ show (buttonNum elem')
          Door -> "d" ++ show (buttonNum elem')
          Exit1 -> "q1"
          Exit2 -> "q2"
          Player1 -> "t1"
          Player2 -> "t1"

addPlayers :: [[Button]] -> String
addPlayers buttons = unwords [addPlayer Player1, addPlayer Player2] ++ "\n"
  where addPlayer player = toStr . fromJust . head . filter isJust . map (findInRow player) . zip [1..] $ buttons
        findInRow player (n, buttonRow)
          = foldr
            (\(m, button) acc -> if isJust acc
                                 then acc
                                 else if buttonState button == player
                                      then Just (n, m)
                                      else Nothing
            )
            Nothing
            $ zip [1..] buttonRow
        toStr (n, m) = show m ++ " " ++ show n

addBack :: [[Button]] -> String
addBack = const "default\n"

addAbilities :: [[Button]] -> String
addAbilities = const "with\n"
