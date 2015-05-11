module Data.Region
    ( R.Rect (..)
    , Region
    , empty
    , everything
    , inside
    , outside
    , add
    , sub
    )
where

import qualified Data.Rect as R
import Linear

data RegionData a =
    Add (R.Rect a)
  | Sub (R.Rect a)
  deriving (Eq, Show)

newtype Region a = Region { getData :: [RegionData a] }
    deriving (Eq, Show)

-- | Checks if a point is inside a region.
inside :: Ord a => V2 a -> Region a -> Bool
inside p = helper True . reverse . getData
    where
        helper t []         = t
        helper t (Add r:rs) = helper (R.inside  p r || t) rs
        helper _ (Sub r:rs) = helper (R.outside p r     ) rs

-- | Checks if a point is outside a region.
outside :: Ord a => V2 a -> Region a -> Bool
outside p = not . inside p

-- | Creates a new empty region.
empty :: Region a
empty = Region []

-- | Creates a region covering everything.
everything :: Bounded a => Region a
everything = Region [Add (R.Rect (V2 minBound minBound) (V2 maxBound maxBound))]

-- | Adds a rectangle to the region.
add :: R.Rect a -> Region a -> Region a
add r (Region rs) = Region (Add r : rs)

-- | Subtracts a rectangle from the region.
sub :: R.Rect a -> Region a -> Region a
sub r (Region rs) = Region (Sub r : rs)
