module Data.STree
    ( STree (..)
    , label
    , singleton
    )
where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Traversable

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
