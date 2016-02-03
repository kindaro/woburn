{-# LANGUAGE TemplateHaskell #-}

module Test.Rect
    ( rectTests
    )
where

import Data.Foldable
import Data.Rect
import Linear
import Test.Arbitrary ()
import Test.QuickCheck

prop_size :: Rect Int -> Property
prop_size r@(Rect (V2 x1 y1) (V2 x2 y2)) =
    let V2 w h = size r
        w' = x2 - x1 + 1
        h' = y2 - y1 + 1
    in
    w' === w .&&. h' === h

prop_inside :: Rect Int -> V2 Int -> Property
prop_inside r@(Rect (V2 x1 y1) (V2 x2 y2)) p@(V2 x y) =
    inside p r === (x >= x1 && x <= x2 && y >= y1 && y <= y2)

prop_outside :: Rect Int -> V2 Int -> Property
prop_outside r@(Rect (V2 x1 y1) (V2 x2 y2)) p@(V2 x y) =
    outside p r === not (x >= x1 && x <= x2 && y >= y1 && y <= y2)

prop_shift :: V2 Int -> Rect Int -> Property
prop_shift off@(V2 offX offY) r@(Rect (V2 x1 y1) (V2 x2 y2)) =
    shift off r === Rect (V2 (x1 + offX) (y1 + offY)) (V2 (x2 + offX) (y2 + offY))

prop_shiftX :: Int -> Rect Int -> Property
prop_shiftX offX r@(Rect (V2 x1 y1) (V2 x2 y2)) =
    shiftX offX r === Rect (V2 (x1 + offX) y1) (V2 (x2 + offX) y2)

prop_shiftY :: Int -> Rect Int -> Property
prop_shiftY offY r@(Rect (V2 x1 y1) (V2 x2 y2)) =
    shiftY offY r === Rect (V2 x1 (y1 + offY)) (V2 x2 (y2 + offY))

prop_overlaps :: Rect Int -> Rect Int -> Property
prop_overlaps a@(Rect (V2 aX1 aY1) (V2 aX2 aY2)) b@(Rect (V2 bX1 bY1) (V2 bX2 bY2)) =
    overlaps a b === (max aX1 bX1 <= min aX2 bX2 && max aY1 bY1 <= min aY2 bY2)

return []
rectTests :: IO Bool
rectTests = $quickCheckAll
