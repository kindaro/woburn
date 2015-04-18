module Data.Rect
    ( Rect (..)
    , inside
    , outside
    )
where

import Linear

-- | An inclusive rectangle.
-- 
-- The first argument is the position of the upper-left corner, the second the
-- lower-right corner.
data Rect a = Rect (V2 a) (V2 a)

instance Functor Rect where
    fmap f (Rect start stop) = Rect (fmap f start) (fmap f stop)

-- | Checks if a point is inside a rectangle.
inside :: Ord a => V2 a -> Rect a -> Bool
inside p (Rect start stop) = p >= start && p <= stop

-- | Checks if a point is outside a rectangle.
outside :: Ord a => V2 a -> Rect a -> Bool
outside p = not . inside p
