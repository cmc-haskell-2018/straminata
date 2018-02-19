module Model.CommonTypes where

import Graphics.Gloss (Color)
import Graphics.Gloss.Interface.IO.Game (Event)

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


type Time = Float


data Hitbox = Hitbox { position :: Position -- ^ Object position
                     , displayBox :: Rectangle
                     , collisionBoxes :: [Rectangle]
                     }
  deriving Show

-- | Contains information about a movable game entity.
data Object = Object { name :: String -- ^ Unique object identifier.
                     , hitbox :: Hitbox -- ^ Object hitbox
                     , velocity :: Vector -- ^ X and Y velocity components.
                     }
  deriving Show

type PlayerControls = (Player -> Event -> Player)

data Player = Player { object :: Object
                     , playerColor :: Color
                     , playerControls :: PlayerControls
                     }

instance Show Player where
  show player = "Player {" ++ show (object player) ++ ", " ++ show (playerColor player) ++ "}"

type Players = [Player]

-- | Describes object movement along a single axis.
type Movement = (Float -> Float)

-- | Describes restrictions imposed on position of an object.
type PositionConstraint = (Float -> Bool)

-- | Represents a game session.
data Game = Game { players :: Players
                 , objects :: [Object]
                 }
