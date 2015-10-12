module Data.Rect
    ( Rect (..)
    , inside
    , outside
    , shiftX
    , shiftY
    , width
    , height
    )
where

import Control.Lens ((%~), (^.))
import Data.Function
import Linear

-- | An inclusive rectangle.
-- 
-- The first argument is the position of the upper-left corner, the second the
-- lower-right corner.
data Rect a =
    Rect { topLeft     :: V2 a
         , bottomRight :: V2 a
         }
    deriving (Eq, Show)

instance Functor Rect where
    fmap f (Rect start stop) = Rect (fmap f start) (fmap f stop)

-- | Returns the width of a rectangle.
width :: Num a => Rect a -> a
width (Rect a b) = b ^. _x - a ^. _x + 1

-- | Returns the height of a rectangle.
height :: Num a => Rect a -> a
height (Rect a b) = b ^. _y - a ^. _y + 1

-- | Checks if a point is inside a rectangle.
inside :: Ord a => V2 a -> Rect a -> Bool
inside p (Rect start stop) = p >= start && p <= stop

-- | Checks if a point is outside a rectangle.
outside :: Ord a => V2 a -> Rect a -> Bool
outside p = not . inside p

-- | Shifts the rectangle along the X-axis.
shiftX :: Num a => a -> Rect a -> Rect a
shiftX d (Rect a b) = (Rect `on` f) a b
    where
        f x = x & _x %~ (+ d)

-- | Shifts the rectangle along the Y-axis.
shiftY :: Num a => a -> Rect a -> Rect a
shiftY d (Rect a b) = (Rect `on` f) a b
    where
        f x = x & _y %~ (+ d)
