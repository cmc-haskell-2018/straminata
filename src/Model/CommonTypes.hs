{-# OPTIONS -XExistentialQuantification #-}
module Model.CommonTypes where

import Graphics.Gloss (Picture)
import Graphics.Gloss.Interface.IO.Game (Key, KeyState(..))

class UnwrapablePair a where
  unwrap :: a -> (Float, Float)

class LSE a where
  plus :: a -> a -> a

  minus :: a -> a -> a
  minus x y = plus x (invert y)

  invert :: a -> a

  mulByNumber :: a -> Float -> a
  mulByNumber x n = divByNumber x (1 / n)

  divByNumber :: a -> Float -> a
  divByNumber x n = mulByNumber x (1 / n)


class ControlType a where
  controlPlayer :: a -> Player -> Player

newtype Position = Position (Float, Float)
  deriving (Show, Eq)

instance UnwrapablePair Position where
  unwrap (Position t) = t

instance LSE Position where
  plus (Position (x1, y1)) (Position (x2, y2)) = Position (x1 + x2, y1 + y2)

  invert (Position (x, y)) = Position (-x, -y)

  mulByNumber (Position (x, y)) number = Position (x * number, y * number)

newtype Dimensions = Dimensions (Float, Float)
  deriving (Show, Eq)

instance (UnwrapablePair Dimensions) where
  unwrap (Dimensions t) = t

type Rectangle = (Position, Position)

type Bounds = Rectangle

newtype Vector = Vector (Float, Float)
  deriving (Show, Eq)

instance (UnwrapablePair Vector) where
  unwrap (Vector t) = t

instance LSE Vector where
  plus (Vector (x1, y1)) (Vector (x2, y2)) = Vector (x1 + x2, y1 + y2)

  invert (Vector (x, y)) = Vector ((- x), (- y))

  divByNumber _ 0 = Vector (0, 0)
  divByNumber (Vector (x, y)) number = Vector (x / number, y / number)

instance ControlType Vector where
  controlPlayer velocity player =
    let object = playerObject player
        currentVelocity = objectVelocity object
    in player { playerObject = object { objectVelocity = velocity `plus` currentVelocity } }

type Time = Float

data Appearance = Appearance
  { appearanceBox :: Rectangle
  , appearanceActualSize :: Dimensions
  , appearancePicture :: Picture
  } deriving (Show)

computeScale :: Rectangle
             -> Dimensions
             -> (Float, Float)
computeScale ((Position (lx, ly)), (Position (rx, ry)))
             (Dimensions (width, height)) = ((abs $ rx - lx) / width, (abs $ ry - ly) / height)

data Hitbox = Hitbox
  { hitboxPosition :: Position -- ^ Object position
  , hitboxCollisionBoxes :: [Rectangle]
  } deriving Show

-- | Contains information about a movable game entity.
data Object = Object
  { objectName :: String -- ^ Unique object identifier.
  , objectHitbox :: Hitbox -- ^ Object hitbox.
  , objectAppearance :: Appearance -- ^ Visual representation.
  , objectVelocity :: Vector -- ^ X and Y velocity components.
  } deriving Show

data ControlElement = forall a. ControlType a => ControlElement a

type DownAction = ControlElement
type UpAction = ControlElement
type KeyPress = (Key, DownAction, UpAction)

upOrDown :: KeyState -> KeyPress -> ControlElement
upOrDown Down (_, x, _) = x
upOrDown Up (_, _, x) = x

instance ControlType ControlElement where
  controlPlayer (ControlElement x) = controlPlayer x

type PlayerControls = [KeyPress]

data Player = Player
  { playerObject :: Object
  , playerControls :: PlayerControls
  }

instance Show Player where
  show player = "Player {" ++ show (playerObject player) ++ "}"

type Players = [Player]

-- | Describes object movement along a single axis.
type Movement = (Float -> Float)

-- | Describes restrictions imposed on position of an object.
type PositionConstraint = (Float -> Bool)

-- | Represents a game session.
data Game = Game
  { gamePlayers :: Players
  , gameObjects :: [Object]
  , gameLevel :: Level
  , gameCamera :: Camera
  } deriving (Show)

data Camera = Camera
  { cameraPosition :: Position
  , cameraRatio :: Float
  } deriving (Show)

data Level = Level
  { levelMap :: Map
  , levelColNumber :: Int
  , levelRowNumber :: Int
  , levelTileSize :: Float
  } deriving (Show)

type Map = [MapRow]

type MapRow = [MapTile]

data MapTile = Solid Object | Transparent Object
  deriving (Show)

unwrapMapTile :: MapTile -> Object
unwrapMapTile (Solid o) = o
unwrapMapTile (Transparent o) = o
