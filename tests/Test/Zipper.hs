{-# LANGUAGE TemplateHaskell #-}
module Test.Zipper
    ( zipperTests
    )
where

import Data.Maybe
import Data.Word
import Data.STree
import Data.STree.Zipper
import Test.Arbitrary ()
import Test.QuickCheck hiding (label)

prop_insert :: Word32 -> [Word32] -> Property
prop_insert root cs =
    let ct = map singleton cs
    in
    toTree (foldr insert (fromTree (singleton root)) ct) === STree ct root []

prop_down :: STree Word32 -> Property
prop_down t =
    case (t, down $ fromTree t) of
         (STree []    _ []   , z     ) -> z === Nothing
         (STree []    _ (r:_), Just z) -> r === getTree z
         (STree (l:_) _ _    , Just z) -> l === getTree z
         (_                  , z     ) -> counterexample (show t ++ ", " ++ show z) False

prop_left_right :: STree Word32 -> Property
prop_left_right t@(STree l _ r) =
    length (l ++ r) >= 2 ==>
        let z = down (fromTree t) in
        (z >>= right >>= left) === z .&&. counterexample ("isJust " ++ show z) (isJust z)

prop_children :: STree Word32 -> Property
prop_children t@(STree l _ r) = l ++ r === map getTree (children $ fromTree t)

return []
zipperTests :: IO Bool
zipperTests = $quickCheckAll
