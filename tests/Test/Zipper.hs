{-# LANGUAGE TemplateHaskell #-}
module Test.Zipper
    ( zipperTests
    )
where

import Data.Foldable
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

prop_down_up :: STree Word32 -> Property
prop_down_up t =
    forAll (walkDown $ fromTree t) $ \u -> getTree (walkUp u) === t
    where
        walkUp z = case up z of
                     Nothing -> z
                     Just z' -> walkUp z'

        walkDown z = case getTree z of
                       STree [] _ [] -> return z
                       STree ls _ rs -> do
                           n <- choose (0, length (ls ++ rs) - 1)
                           let Just z' = foldl' (>>=) (down z) (replicate n right)
                           walkDown z'

prop_left_right_1 :: STree Word32 -> Property
prop_left_right_1 t@(STree l _ r) =
    length (l ++ r) >= 2 ==>
        let z = down (fromTree t) in
        (z >>= right >>= left) === z .&&. counterexample ("isJust " ++ show z) (isJust z)

prop_left_right_2 :: STree Word32 -> Property
prop_left_right_2 t@(STree l _ r) =
    length (l ++ r) >= 1 ==>
        let Just z      = down (fromTree t)
            (z' , lToR) = walk right [] z
            (z'', rToL) = walk left  [] z'
        in
        rToL === l ++ r .&&. lToR === reverse (l ++ r) .&&. z'' === z
    where
        walk f ls z =
            let ls' = getTree z : ls
            in
            case f z of
              Nothing -> (z, ls')
              Just z' -> walk f ls' z'

prop_children :: STree Word32 -> Property
prop_children t@(STree l _ r) = l ++ r === map getTree (children $ fromTree t)

return []
zipperTests :: IO Bool
zipperTests = $quickCheckAll
