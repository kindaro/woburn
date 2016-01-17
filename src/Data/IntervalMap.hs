module Data.IntervalMap
    ( Interval (..)
    , IntervalMap
    , empty
    , lookup
    , insert
    , delete
    , toList
    , mapValues
    , mapIntervals
    )
where

import Control.Applicative hiding (empty)
import Data.Maybe
import Data.Set.Diet (Interval (..), point)
import qualified Data.Map.Strict as M
import Prelude hiding (lookup)

-- | A map from closed interval to values.
--
-- The intervals may not overlap.
newtype IntervalMap a b = IntervalMap { unIntervalMap :: M.Map (Interval a) b }
    deriving (Eq, Ord, Show)

-- | Returns an emtpy 'IntervalMap'.
empty :: IntervalMap a b
empty = IntervalMap M.empty

-- | Finds all intervals that overlap with the given interval, as well as their
-- associated values.
--
-- This has a rather nasty upper limit on /O(n log n)/, but runs in /O(log n)/
-- on point intervals.
lookup :: (Bounded a, Ord a) => Interval a -> IntervalMap a b -> [(Interval a, b)]
lookup i@(Interval low high) m =
    fromMaybe []
    $ helper
    <$> (M.lookupLE i (unIntervalMap m) <|> M.lookupGT i (unIntervalMap m))
    where
        helper j@(Interval prev next, _)
            | prev > high = []
            | next >= low = j : xs
            | otherwise   = xs
            where
                xs = fromMaybe [] (helper <$> M.lookupGT (point next) (unIntervalMap m))

-- | Inserts an 'Interval' and its associated value into the map. 
insert :: Ord a => Interval a -> b -> IntervalMap a b -> IntervalMap a b
insert a b = IntervalMap . M.insert a b . unIntervalMap

-- | Deletes an 'Interval' from the map.
delete :: Ord a => Interval a -> IntervalMap a b -> IntervalMap a b
delete k = IntervalMap . M.delete k . unIntervalMap

-- | Returns the set as a list.
toList :: IntervalMap a b -> [(Interval a, b)]
toList = M.toList . unIntervalMap

-- | Applies a function to all the values in the map.
mapValues :: (a -> b) -> IntervalMap k a -> IntervalMap k b
mapValues f = IntervalMap . M.map f . unIntervalMap

-- | Applies a function to all the intervals.
--
-- Note that the function @f@ must be strictly increasing, that is if @x < y@,
-- then @f x < f y@. This condition is not checked.
mapIntervals :: (Interval a -> Interval b) -> IntervalMap a v -> IntervalMap b v
mapIntervals f = IntervalMap . M.mapKeysMonotonic f . unIntervalMap
