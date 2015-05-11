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

prop_goDown :: STree Word32 -> Property
prop_goDown t =
    case (t, goDown $ fromTree t) of
         (STree []    _ []   , z     ) -> z === Nothing
         (STree []    _ (r:_), Just z) -> r === getTree z
         (STree (l:_) _ _    , Just z) -> l === getTree z
         (_                  , z     ) -> counterexample (show t ++ ", " ++ show z) False

prop_goLeftRight :: STree Word32 -> Property
prop_goLeftRight t@(STree l _ r) =
    length (l ++ r) >= 2 ==>
        let z = goDown (fromTree t) in
        (z >>= goRight >>= goLeft) === z .&&. counterexample ("isJust " ++ show z) (isJust z)

prop_children :: STree Word32 -> Property
prop_children t@(STree l _ r) = l ++ r === map getTree (children $ fromTree t)

return []
zipperTests :: IO Bool
zipperTests = $quickCheckAll
