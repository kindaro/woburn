module Data.Region
    ( R.Rect (..)
    , Region
    , empty
    , everything
    , inside
    , outside
    , add
    , sub
    , scale
    , offset
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

instance Functor RegionData where
    fmap f (Add r) = Add $ fmap f r
    fmap f (Sub r) = Sub $ fmap f r

instance Functor Region where
    fmap f (Region rs) = Region $ map (fmap f) rs

instance Num a => Monoid (Region a) where
    mempty = empty
    mappend (Region as) (Region bs) = Region (as ++ bs)

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

-- | Scales a region.
scale :: Num a => a -> Region a -> Region a
scale s = fmap (* s)

-- | Offsets a region.
offset :: Num a => V2 a -> Region a -> Region a
offset off (Region rs) = Region $ map f rs
    where
        f (Add r) = Add (R.shift off r)
        f (Sub r) = Sub (R.shift off r)
