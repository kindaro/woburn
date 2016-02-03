module Data.Rect
    ( Rect (..)
    , inside
    , outside
    , overlaps
    , shiftX
    , shiftY
    , shift
    , width
    , height
    , size
    )
where

import Control.Monad.Zip
import Control.Lens ((^.))
import Data.Function
import qualified Data.Set.Diet as D
import Linear hiding (project)

-- | An inclusive rectangle.
--
-- The first argument is the position of the upper-left corner, the second the
-- lower-right corner.
data Rect a =
    Rect { topLeft     :: V2 a
         , bottomRight :: V2 a
         }
    deriving (Eq, Ord, Show)

instance Functor Rect where
    fmap f (Rect start stop) = Rect (fmap f start) (fmap f stop)

-- | Returns the width of a rectangle.
width :: Num a => Rect a -> a
width (Rect a b) = b ^. _x - a ^. _x + 1

-- | Returns the height of a rectangle.
height :: Num a => Rect a -> a
height (Rect a b) = b ^. _y - a ^. _y + 1

-- | Returns the size of a rectangle.
size :: Num a => Rect a -> V2 a
size r = V2 (width r) (height r)

-- | Checks if a point is inside a rectangle.
inside :: Ord a => V2 a -> Rect a -> Bool
inside p (Rect start stop) =
    and (mzipWith (>=) p start) &&
    and (mzipWith (<=) p stop)

-- | Checks if a point is outside a rectangle.
outside :: Ord a => V2 a -> Rect a -> Bool
outside p = not . inside p

-- | Checks if two rectangles overlap.
overlaps :: Ord a => Rect a -> Rect a -> Bool
overlaps a b =
    on D.overlapping (project _x) a b && on D.overlapping (project _y) a b
    where
        -- | Projects a rectangle onto the x- or y-axis.
        project lens r = D.Interval (topLeft r ^. lens) (bottomRight r ^. lens)

-- | Shifts the rectangle along the X-axis.
shiftX :: Num a => a -> Rect a -> Rect a
shiftX d = shift (V2 d 0)

-- | Shifts the rectangle along the Y-axis.
shiftY :: Num a => a -> Rect a -> Rect a
shiftY d = shift (V2 0 d)

-- | Shifts the rectangle.
shift :: Num a => V2 a -> Rect a -> Rect a
shift off (Rect a b) = Rect (a + off) (b + off)
