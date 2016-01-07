module Test.Arbitrary
    ()
where

import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Data.STree
import Data.Word
import Prelude
import Woburn.Surface
import Test.QuickCheck

instance Arbitrary SurfaceId where
    arbitrary = fromIntegral <$> (arbitrary :: Gen Word32)
    shrink s = fromIntegral <$> shrink (fromIntegral s :: Word32)

instance (Num a) => Arbitrary (STree a) where
    arbitrary = sized ((`evalStateT` 0) . f)
        where
            nextElem = state (id &&& (+ 1))
            f n
              | n <= 1    = STree <$> pure [] <*> nextElem <*> pure []
              | otherwise = do
                l <- lift $ choose (0, n - 1)
                r <- lift $ choose (0, n - 1 - l)
                let n' = (n - 1) `div` (l + r)
                STree
                    <$> replicateM l (f n')
                    <*> nextElem
                    <*> replicateM r (f n')

    shrink (STree [] _ []) = []
    shrink (STree l  n r ) =
        [STree [] n []]
        ++ l
        ++ r
        ++ [STree l' n r' | (l', r') <- shrink (l, r)]

instance Arbitrary a => Arbitrary (Surface a) where
    arbitrary = do
        surf <- create <$> arbitrary
        sync <- arbitrary
        return surf { surfSync = sync }
