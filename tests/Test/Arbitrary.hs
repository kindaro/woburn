module Test.Arbitrary
    ()
where

import Control.Applicative
import Control.Arrow
import Data.Foldable (toList)
import Data.List (nub)
import Data.STree
import Data.Word
import Woburn.Surface
import Test.QuickCheck

instance Arbitrary SurfaceId where
    arbitrary = fromIntegral <$> (arbitrary :: Gen Word32)
    shrink s = fromIntegral <$> shrink (fromIntegral s :: Word32)

instance (Eq a, Arbitrary a) => Arbitrary (STree a) where
    arbitrary = suchThat (sized f) (uncurry (==) . (id &&& nub) . toList)
        where
            f 0 = STree <$> pure [] <*> arbitrary <*> pure []
            f n = do
                l <- choose (0, n)
                r <- choose (0, n - l)
                let n' = n `div` (l + r)
                STree
                    <$> vectorOf l (f n')
                    <*> arbitrary
                    <*> vectorOf r (f n')

    shrink (STree [] _ []) = []
    shrink (STree l  n r ) = [STree [] n []] ++ l ++ r

instance Arbitrary a => Arbitrary (Surface a) where
    arbitrary = do
        surf <- create <$> arbitrary
        sync <- arbitrary
        return surf { surfSync = sync }
