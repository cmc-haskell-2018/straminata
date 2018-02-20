{-# OPTIONS -XExistentialQuantification #-}
module Model.CommonTypes where

import Graphics.Gloss (Color)
import Graphics.Gloss.Interface.IO.Game (Key, KeyState(..))

class UnwrapablePair a where
  unwrap :: a -> (Float, Float)


newtype Position = Position (Float, Float)
  deriving (Show, Eq)

instance (UnwrapablePair Position) where
  unwrap (Position t) = t


newtype Dimensions = Dimensions (Float, Float)
  deriving (Show, Eq)

instance (UnwrapablePair Dimensions) where
  unwrap (Dimensions t) = t

-- todo: refactor
newtype Rectangle = Rectangle (Position, Position)
  deriving (Show, Eq)

unwrapRectangle :: Rectangle -> (Position, Position)
unwrapRectangle (Rectangle t) = t

type Bounds = Rectangle

newtype Vector = Vector (Float, Float)
  deriving (Show, Eq)

instance (UnwrapablePair Vector) where
  unwrap (Vector t) = t

plus :: Vector -> Vector -> Vector
plus (Vector (x1, y1)) (Vector (x2, y2)) = Vector (x1 + x2, y1 + y2)

instance ControlType Vector where
  controlPlayer velocity player =
    let object = playerObject player
        currentVelocity = objectVelocity object
    in player { playerObject = object { objectVelocity = velocity `plus` currentVelocity } }


type Time = Float


data Hitbox = Hitbox
  { hitboxPosition :: Position -- ^ Object position
  , hitboxDisplayBox :: Rectangle
  , hitboxCollisionBoxes :: [Rectangle]
  }
  deriving Show

-- | Contains information about a movable game entity.
data Object = Object
  { objectName :: String -- ^ Unique object identifier.
  , objectHitbox :: Hitbox -- ^ Object hitbox
  , objectVelocity :: Vector -- ^ X and Y velocity components.
  }
  deriving Show

type DownAction = ControlElement
type UpAction = ControlElement
type KeyPress = (Key, DownAction, UpAction)

upOrDown :: KeyState -> KeyPress -> ControlElement
upOrDown Down (_, x, _) = x
upOrDown Up (_, _, x) = x

class ControlType a where
  controlPlayer :: a -> Player -> Player

data ControlElement = forall a. ControlType a => ControlElement a

instance ControlType ControlElement where
  controlPlayer (ControlElement x) = controlPlayer x

type PlayerControls = [KeyPress]

data Player = Player
  { playerObject :: Object
  , playerColor :: Color
  , playerControls :: PlayerControls
  }

instance Show (Player) where
  show player = "Player {" ++ show (playerObject player) ++ ", " ++ show (playerColor player) ++ "}"

type Players = [Player]

-- | Describes object movement along a single axis.
type Movement = (Float -> Float)

-- | Describes restrictions imposed on position of an object.
type PositionConstraint = (Float -> Bool)

-- | Represents a game session.
data Game = Game
  { gamePlayers :: Players
  , gameObjects :: [Object]
  }
