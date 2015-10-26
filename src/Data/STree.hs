module Data.STree
    ( STree (..)
    , label
    , singleton
    , drawTree
    )
where

import Control.Applicative
import Data.Foldable hiding (concatMap)
import Data.Monoid
import Data.Traversable
import Prelude

data STree a = STree [STree a] a [STree a]
    deriving (Eq, Show)

instance Functor STree where
    fmap f (STree l n r) = STree (fmap (fmap f) l) (f n) (fmap (fmap f) r)

instance Foldable STree where
    foldMap f (STree l n r) = mconcat $ map (foldMap f) l ++ [f n] ++ map (foldMap f) r

instance Traversable STree where
    traverse f (STree l n r) =
        STree
        <$> traverse (traverse f) l
        <*> f n
        <*> traverse (traverse f) r

-- | Return the label value.
label :: STree a -> a
label (STree _ x _) = x

-- | Creates a tree with a single element.
singleton :: a -> STree a
singleton a = STree [] a []

-- | Draws a nice 2-dimensional drawing of a tree.
drawTree :: STree String -> String
drawTree = f 0 '*'
    where
        f indent c (STree l n r) =
            concatMap (f (indent + 1) '/') l
            ++ replicate indent '*' ++ [c] ++ n ++ "\n"
            ++ concatMap (f (indent + 1) '\\') r
