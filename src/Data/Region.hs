module Data.Region
    ( R.Rect (..)
    , Region
    , empty
    , everything
    , inside
    , outside
    , add
    , sub
    , rectangles
    , scale
    , offset
    , union
    )
where

import Control.Arrow
import Data.Foldable
import qualified Data.IntervalMap as I
import qualified Data.Rect as R
import qualified Data.Set.Diet as D
import Linear

type Band a = D.Diet a
newtype Region a = Region { unRegion :: I.IntervalMap a (Band a) }
    deriving (Show, Eq)

-- | Creates a new empty region.
empty :: Region a
empty = Region I.empty

-- | Creates a region covering everything.
everything :: (Ord a, Enum a, Bounded a) => Region a
everything = add (R.Rect (V2 minBound minBound) (V2 maxBound maxBound)) empty

-- | Checks if a point is inside a region.
inside :: (Bounded a, Ord a) => V2 a -> Region a -> Bool
inside (V2 x y) r =
    case I.lookup (D.point x) (unRegion r) of
      []       -> False
      [(_, b)] -> D.member y b
      _        -> error "A point interval should return 0 or 1 elements"

-- | Checks if a point is outside a region.
outside :: (Bounded a, Ord a) => V2 a -> Region a -> Bool
outside v = not . inside v

-- | Modifies all the bands an interval intersects.
modifyBands :: (Bounded a, Ord a)
            => I.Interval a
            -> ([(I.Interval a, Band a)] -> [(I.Interval a, Band a)])
            -> Region a
            -> Region a
modifyBands interval f region =
    Region . foldl' (\m (i, b) -> I.insert i b m) region' $ f bands
    where
        -- All bands that are intersected.
        bands   = I.lookup interval (unRegion region)
        -- The region with the old bands removed.
        region' = foldl' (flip I.delete) (unRegion region) (map fst bands)

-- | Splits a band into the parts that intersects with the given interval, that
-- parts that don't, and the parts of the interval that are outside the band.
splitBand :: (Enum a, Ord a)
          => I.Interval a
          -> (I.Interval a, b)
          -> ([(I.Interval a, b)], [(I.Interval a, b)], [I.Interval a])
splitBand a@(I.Interval a1 a2) (b@(I.Interval b1 b2), band) =
    (overlap, old, new)
    where
        overlap = [(I.Interval (max a1 b1) (min a2 b2), band) | D.overlapping a b]

        old = [(I.Interval b1                 (min b2 (pred a1)), band) | b1 < a1] ++
              [(I.Interval (max b1 (succ a2)) b2                , band) | b2 > a2]

        new = [ I.Interval a1                 (min a2 (pred b1)) | a1 < b1] ++
              [ I.Interval (max a1 (succ b2)) a2                 | a2 > b2]

-- | Adds a rectangle to the region.
add :: (Ord a, Bounded a, Enum a) => R.Rect a -> Region a -> Region a
add (R.Rect (V2 x1 y1) (V2 x2 y2)) = modifyBands (I.Interval x1 x2) (insertBand x1)
    where
        newBand = D.insertI (I.Interval y1 y2) D.empty

        -- Inserts the interval (y1, y2) into all the bands, splitting the
        -- bands up if the new rectangle only partly covers them.
        insertBand x []
            | x <= x2   = [(I.Interval x x2, newBand)]
            | otherwise = []
        insertBand x (band@(I.Interval _ xh, _) : xs) =
            old ++
            map (\a -> (a, newBand)) new ++
            map (second (D.insertI (D.Interval y1 y2))) overlap ++
            insertBand (succ xh) xs
            where
                (overlap, old, new) = splitBand (I.Interval x (min x2 xh)) band

-- | Subtracts a rectangle from the region.
sub :: (Ord a, Bounded a, Enum a) => R.Rect a -> Region a -> Region a
sub (R.Rect (V2 x1 y1) (V2 x2 y2)) = modifyBands (I.Interval x1 x2) (filter (not . D.null . snd) . deleteBand)
    where
        -- Deletes the interval from all the overlapping bands.
        deleteBand [] = []
        deleteBand (band : xs) =
            old ++ map (second (D.deleteI (D.Interval y1 y2))) overlap ++ deleteBand xs
            where
                (overlap, old, _) = splitBand (I.Interval x1 x2) band

-- | Returns a list of non-overlapping rectangles describing the region.
rectangles :: Region a -> [R.Rect a]
rectangles = concatMap bandToList . I.toList . unRegion
    where
        bandToList (I.Interval x1 x2, b) =
            map (\(I.Interval y1 y2) -> R.Rect (V2 x1 y1) (V2 x2 y2)) $ D.toListI b

-- | Scales a region.
--
-- Scaling a region by a non-positive number has undefined result.
--
-- Note that, unless @a@ is an infinte type like 'Integer', scaling has
-- undefined result if it results in overflows. This is deemed acceptable for
-- Woburn, since this is likely to be used to scale damage buffers which should
-- be represented with 'Int32', and it's unlikely we'll see windows that have
-- sizes within a small scale factor of @2^31@.
scale :: (Ord a, Num a) => a -> Region a -> Region a
scale s =
    Region
    . I.mapValues (D.mapI f)
    . I.mapIntervals f
    . unRegion
    where
        f = fmap (* s)

-- | Offsets a region.
offset :: (Ord a, Num a) => V2 a -> Region a -> Region a
offset (V2 offX offY) =
    Region
    . I.mapValues (D.mapI (fmap (+ offY)))
    . I.mapIntervals (fmap (+ offX))
    . unRegion

-- | Returns the union of two regions.
union :: (Bounded a, Ord a, Enum a) => Region a -> Region a -> Region a
union a = foldl' (flip add) a . rectangles
