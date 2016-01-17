{-# LANGUAGE TemplateHaskell #-}

module Test.Region
    ( regionTests
    )
where

import Data.Foldable
import Data.Region
import qualified Data.Rect as R
import Linear
import Test.Arbitrary ()
import Test.QuickCheck hiding (scale)

data Op = Add | Sub
    deriving (Eq, Ord, Show, Enum, Bounded)

instance Arbitrary Op where
    arbitrary = elements [Add, Sub]

prop_empty :: V2 Int -> Property
prop_empty v =
    counterexample "v not outside empty" (v `outside` empty) .&&.
    counterexample "v inside empty" (not (v `inside` empty))

prop_everything :: V2 Int -> Property
prop_everything v =
    counterexample "v outside everything" (not (v `outside` everything)) .&&.
    counterexample "v not inside everything" (v `inside` everything)

isInside :: V2 Int -> [(Op, Rect Int)] -> Bool
isInside v = foldl' f False
    where
        f t (Add, rect) = t || R.inside v rect
        f t (Sub, rect) = t && R.outside v rect

mkRegion :: [(Op, Rect Int)] -> Region Int
mkRegion = foldl' f empty
    where
        f region (Add, rect) = add rect region
        f region (Sub, rect) = sub rect region

prop_addSub :: V2 Int -> [(Op, Rect Int)] -> Property
prop_addSub v rs =
    let reg = mkRegion rs
    in
    v `isInside` rs === v `inside` reg .&&.
    not (v `isInside` rs) === v `outside` reg

prop_union :: V2 Int -> [(Op, Rect Int)] -> [(Op, Rect Int)] -> Property
prop_union v as bs =
    let regA = mkRegion as
        regB = mkRegion bs
        reg  = regA `union` regB
    in
    (v `isInside` as || v `isInside` bs) === v `inside` reg

prop_scale :: Int -> V2 Int -> [(Op, Rect Int)] -> Property
prop_scale s p rs =
    let reg = mkRegion rs
    in
    s > 0 ==> p `inside` reg === (p ^* s) `inside` scale s reg

prop_offset :: V2 Int -> V2 Int -> [(Op, Rect Int)] -> Property
prop_offset off p rs =
    let reg = mkRegion rs
    in
    p `inside` reg === (p + off) `inside` offset off reg

return []
regionTests :: IO Bool
regionTests = $quickCheckAll
