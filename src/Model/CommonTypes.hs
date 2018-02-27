{-# OPTIONS -XExistentialQuantification #-}
module Model.CommonTypes where

import Graphics.Gloss (Picture)
import Graphics.Gloss.Interface.IO.Game (Key, KeyState(..))

class UnwrapablePair a where
  unwrap :: a -> (Float, Float)

class ControlType a where
  controlPlayer :: a -> Player -> Player

newtype Position = Position (Float, Float)
  deriving (Show, Eq)

instance (UnwrapablePair Position) where
  unwrap (Position t) = t

vectorToPosition :: Vector -> Position
vectorToPosition v = Position (fst . unwrap $ v, snd . unwrap $ v)

positionToVector :: Position -> Vector
positionToVector p = Vector (fst . unwrap $ p, snd . unwrap $ p)

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

plus :: Vector -> Vector -> Vector
plus (Vector (x1, y1)) (Vector (x2, y2)) = Vector (x1 + x2, y1 + y2)

divByNumber :: Vector -> Float -> Vector
divByNumber _ 0 = Vector (0, 0)
divByNumber (Vector (x, y)) number = Vector (x / number, y / number)

invertVector :: Vector -> Vector
invertVector (Vector (x, y)) = Vector ((- x), (- y))

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

-- | Contains information about a movable game entity.
data Object = Object
  { objectName :: String -- ^ Unique object identifier.
  , objectPosition :: Position -- ^ Object position
  , objectCollisionBoxes :: [Rectangle]
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
  , levelObjects :: [Object]
  , levelBackground :: Appearance
  } deriving (Show)

type Map = [MapRow]

type MapRow = [Tile]

data TileType = Solid | Transparent
  deriving (Show)

data Tile = Tile
  { tileType :: TileType
  , tileObject :: Object
  } deriving (Show)

type Texture = (Dimensions, Picture)
