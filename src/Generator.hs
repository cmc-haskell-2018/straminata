module Generator where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Model.CommonTypes
import Visual.WindowConstants

data Generator = Generator
  { generatorButtons :: [[Button]]
  }

data ButtonState = None | Grass | Dirt | Coin -- | Button | Door
  deriving (Enum)

nextState :: ButtonState -> ButtonState
nextState Coin = None
nextState s = succ s

stateColor :: ButtonState -> Color
stateColor None = white
stateColor Grass = green
stateColor Dirt = black
stateColor Coin = yellow

data Button = Button
  { buttonPos :: Position
  , buttonState :: ButtonState
  }

maximumMapSize :: Float
maximumMapSize = 30

generateEmptyButtons :: [[Button]]
generateEmptyButtons = map (scaleRow buttonLinearSize) rowSetter
  where
    rowSetter = createWithNumber maximumMapSize (\r n -> map (setterY n) r) row
    row = createWithNumber maximumMapSize setterX $ Button { buttonPos = Position (0, 0), buttonState = None }
    setterX button x = button { buttonPos = setX x (buttonPos button) }
    setterY y button = button { buttonPos = setY y (buttonPos button) }
    setX x (Position (_, y)) = Position (x, y)
    setY y (Position (x, _)) = Position (x, y)
    scaleRow times = map (\b -> b { buttonPos = multPos times (buttonPos b) })
    multPos times (Position (x, y)) = Position (x * times, y * times)

createWithNumber :: Float -> (a -> Float -> a) -> a -> [a]
createWithNumber maxNum setter base = map (setter base) [-maxNum / 2 .. maxNum / 2]

initialGeneratorState :: Generator
initialGeneratorState = Generator
  { generatorButtons = generateEmptyButtons
  }

buttonSize :: Dimensions
buttonSize = Dimensions $ double . (/ maximumMapSize) . fromIntegral . (\x -> x - 100) . snd $ initialWindowDimensions
  where double x = (x, x)

buttonLinearSize :: Float
buttonLinearSize = fst . unwrap $ buttonSize

renderGenerator :: Generator -> Picture
renderGenerator generator = Pictures
                          $ map (draw) (concat . generatorButtons $ generator)
                          ++ map (borders) (concat . generatorButtons $ generator)
  where draw button = Color (stateColor . buttonState $ button)
                    $ uncurry Translate (unwrap . buttonPos $ button)
                    $ uncurry rectangleSolid (unwrap $ buttonSize)
        borders button = Color cyan
                       $ uncurry Translate (unwrap . buttonPos $ button)
                       $ uncurry rectangleWire (unwrap $ buttonSize)

handleGeneratorInput :: Event -> Generator -> Generator
handleGeneratorInput (EventKey (MouseButton LeftButton) Up _ (x, y)) generator = generator
  { generatorButtons = map (map click) $ generatorButtons generator
  }
  where click button = if hit button then button { buttonState = nextState . buttonState $ button } else button
        hit b = let (x', y') = unwrap $ buttonPos b
                in x >= x' - buttonLinearSize / 2
                && x <= x' + buttonLinearSize / 2
                && y >= y' - buttonLinearSize / 2
                && y <= y' + buttonLinearSize / 2
handleGeneratorInput _ generator = generator

advanceGenerator :: Float -> Generator -> Generator
advanceGenerator _ generator = generator
