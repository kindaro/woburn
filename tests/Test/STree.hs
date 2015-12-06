{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Test.STree
    ( sTreeTests
    )
where

import Control.Applicative
import Control.Arrow
import Control.Monad.Writer
import qualified Data.DList as DL
import Data.Foldable
import Data.Traversable
import Data.STree
import Data.Word
import Test.Arbitrary ()
import Test.QuickCheck hiding (label, shuffle)

treeToList :: STree a -> [a]
treeToList (STree ls n rs) = concatMap treeToList ls ++ [n] ++ concatMap treeToList rs

prop_foldl :: STree Word32 -> Property
prop_foldl st = foldl (flip (:)) [] st === reverse (treeToList st)

prop_foldr :: STree Word32 -> Property
prop_foldr st = foldr (:) [] st === treeToList st

prop_traverse :: STree Word32 -> Property
prop_traverse st =
    let (st', ls) = runWriter (traverse (\a -> tell (DL.singleton a) >> return a) st)
    in
    st' === st .&&. toList ls == treeToList st

return []
sTreeTests :: IO Bool
sTreeTests = $quickCheckAll
