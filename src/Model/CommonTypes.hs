{-# OPTIONS -XExistentialQuantification #-}
module Model.CommonTypes where

import Graphics.Gloss (Picture)
import Graphics.Gloss.Interface.IO.Game (Key)

import Util.Constants

class UnwrapablePair a where
  unwrap :: a -> (Float, Float)

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

infiniteRectangle :: Rectangle
infiniteRectangle = (Position (-infinity, -infinity), Position(infinity, infinity))

getDimensions :: Rectangle -> Dimensions
getDimensions (Position (x1, y1), Position(x2, y2)) = Dimensions (x2 - x1, y2 - y1)

type Line = (Position, Position)

type Bounds = Rectangle

newtype Vector = Vector (Float, Float)
  deriving (Show, Eq)

instance (UnwrapablePair Vector) where
  unwrap (Vector t) = t

getY :: Vector -> Float
getY (Vector (_, y)) = y

getX :: Vector -> Float
getX (Vector (x, _)) = x

plus :: Vector -> Vector -> Vector
plus (Vector (x1, y1)) (Vector (x2, y2)) = Vector (x1 + x2, y1 + y2)

subtractVector :: Vector -> Vector -> Vector
subtractVector v1 v2 = v1 `plus` invertVector v2

divByNumber :: Vector -> Float -> Vector
divByNumber _ 0 = Vector (0, 0)
divByNumber (Vector (x, y)) number = Vector (x / number, y / number)

mulByNumber :: Vector -> Float -> Vector
mulByNumber _ 0 = Vector (0, 0)
mulByNumber (Vector (x, y)) number = Vector (x * number, y * number)

invertVector :: Vector -> Vector
invertVector (Vector (x, y)) = Vector ((- x), (- y))

vectorLength :: Vector -> Float
vectorLength (Vector (x, y)) = sqrt (x * x + y * y)

normalizeVector :: Vector -> Vector
normalizeVector vector = vector `divByNumber` vectorLength vector

takeShortest :: Vector -> Vector -> Vector
takeShortest v1 v2 = if (vectorLength v1) > (vectorLength v2)
                     then v2
                     else v1

projectVector :: Vector -> Vector -> Vector
projectVector _ (Vector (0, 0)) = zeroVector
projectVector v1 v2 = v2 `mulByNumber` ((v1 `dotProduct` v2) / (v2 `dotProduct` v2))

dotProduct :: Vector -> Vector -> Float
dotProduct (Vector (x1, y1)) (Vector (x2, y2)) = x1 * x2 + y1 * y2

angleCosBetweenVectors :: Vector -> Vector -> Float
angleCosBetweenVectors v1 v2 = (v1 `dotProduct` v2) / (vectorLength v1 * vectorLength v2)

perpendicularVector :: Vector -> Vector
perpendicularVector (Vector (x, y)) = Vector (y, -x)

defineDescartesFourth :: Vector -> Int
defineDescartesFourth (Vector (x, y)) =
  if x >= 0
  then if y >= 0 then 1 else 4
  else if y >= 0 then 2 else 3

zeroVector :: Vector
zeroVector = Vector (0, 0)

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
  , objectOnUpdate :: Object -> Game -> Game
  , objectOnActivate :: Bool -> Object -> Game -> Game
  , objectMass :: Float -- ^ Mass. Can be infinite (1/0)
  , objectAcceleration :: Vector
  }

instance Eq Object where
  (==) o1 o2 = objectName o1 == objectName o2

instance Show Object where
  show object = "Object { objectName = " ++ show (objectName object)
                ++ "objectPosition = " ++ show (objectPosition object)
                ++ "objectCollisionBoxes = " ++ show (objectCollisionBoxes object)
                ++ "objectAppearance = " ++ show (objectAppearance object)
                ++ "objectVelocity = " ++ show (objectVelocity object)
                ++ "}"


data Action = PlayerAction (Player -> Game -> Player)
            | GameAction (Player -> Game -> Game)

performOnPlayer :: Action -> Player -> Game -> Player
performOnPlayer (PlayerAction f) player game = f player game
performOnPlayer _ player _ = player

performOnGame :: Action -> Player -> Game -> Game
performOnGame (GameAction f) player game = f player game
performOnGame _ _ game = game

type DownAction = Action
type UpAction = Action
type KeyPress = (Key, DownAction, UpAction)

type PlayerControls = [KeyPress]

data Player = Player
  { playerObject :: Object
  , playerControls :: PlayerControls
  , playerControlVector :: Vector
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

data Tile = Solid Appearance | Transparent Appearance
  deriving (Show)

instance Eq Tile where
  (==) (Solid _) (Transparent _) = False
  (==) (Transparent _) (Solid _) = False
  (==) (Solid a1) (Solid a2) = appearanceBox a1 == appearanceBox a2
  (==) (Transparent a1) (Transparent a2) = appearanceBox a1 == appearanceBox a2

getAppearance :: Tile -> Appearance
getAppearance (Solid app) = app
getAppearance (Transparent app) = app

type Texture = (Dimensions, Picture)
