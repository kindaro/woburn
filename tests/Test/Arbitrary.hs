{-# LANGUAGE FlexibleInstances #-}
module Test.Arbitrary
    ( Tree (..)
    , label
    )
where

import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Data.Monoid
import Data.Rect
import Data.STree hiding (label)
import Data.Word
import Graphics.Wayland
import Linear
import Prelude
import Woburn.Surface
import Test.QuickCheck hiding (label)

instance Arbitrary SurfaceId where
    arbitrary = fromIntegral <$> (arbitrary :: Gen Word32)
    shrink s = fromIntegral <$> shrink (fromIntegral s :: Word32)

instance Arbitrary (Object a b) where
    arbitrary = Object . ObjId <$> arbitrary

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

instance Arbitrary a => Arbitrary (Surface a b) where
    arbitrary = create <$> arbitrary

instance (Ord a, Num a, Arbitrary a) => Arbitrary (Rect a) where
    arbitrary = do
        pos <- arbitrary
        off <- arbitrary `suchThat` (\(V2 x y) -> x >= 0 && y >= 0)
        return $ Rect pos (pos + off)

instance Arbitrary a => Arbitrary (V2 a) where
    arbitrary = V2 <$> arbitrary <*> arbitrary

data Tree a = Tree [Tree a] a [Tree a]
    deriving (Show, Eq)

label :: Tree a -> a
label (Tree _ l _) = l

instance (Num a, Arbitrary b) => Arbitrary (Tree (a, b)) where
    arbitrary = sized ((`evalStateT` 0) . f)
        where
            nextElem =
                (,)
                <$> state (id &&& (+ 1))
                <*> lift arbitrary

            f n
              | n <= 1    = Tree <$> pure [] <*> nextElem <*> pure []
              | otherwise = do
                l <- lift $ choose (0, n - 1)
                r <- lift $ choose (0, n - 1 - l)
                let n' = (n - 1) `div` (l + r)
                Tree
                    <$> replicateM l (f n')
                    <*> nextElem
                    <*> replicateM r (f n')

    shrink (Tree [] _ []) = []
    shrink (Tree l  n r ) =
        [Tree [] n []]
        ++ l
        ++ r
        ++ [Tree l' n r' | (l', r') <- shrink (l, r)]

instance Functor Tree where
    fmap f (Tree l n r) = Tree (map (fmap f) l) (f n) (map (fmap f) r)

instance Foldable Tree where
    foldMap f (Tree l n r) = foldMap (foldMap f) l <> f n <> foldMap (foldMap f) r
