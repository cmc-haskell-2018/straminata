module Model.CommonTypes where

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
newtype Rectangle = Rectangle (Position, Position, Position, Position)
  deriving (Show, Eq)

unwrapRectangle :: Rectangle -> (Position, Position, Position, Position)
unwrapRectangle (Rectangle t) = t

type Bounds = Rectangle


newtype Scaling = Scaling (Float, Float)
  deriving (Show, Eq)

instance (UnwrapablePair Scaling) where
  unwrap (Scaling t) = t

newtype Vector = Vector (Float, Float)
  deriving (Show, Eq)

instance (UnwrapablePair Vector) where
  unwrap (Vector t) = t

-- | Contains information about a movable game entity.
data Object = Object { name :: String -- ^ Unique object identifier.
                     , position :: Position -- ^ Center of a rectangle.
                     , dimensions :: Dimensions -- ^ Half of rectangle width and height.
                     , scaling :: Scaling -- todo 16.02.18: describe scaling parameter
                     , velocity :: Vector -- ^ X and Y velocity components.
                     }

type Player = Object

-- | Describes object movement along a single axis.
type Movement = (Float -> Float)

-- | Describes restrictions imposed on position of an object.
type PositionConstraint = (Float -> Bool)

-- | Represents a game session.
data Game = Game { objects :: [Object]
                 }
