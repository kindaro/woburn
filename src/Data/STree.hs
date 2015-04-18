module Data.STree
    ( STree (..)
    , label
    )
where

data STree a = STree [STree a] a [STree a]
    deriving (Eq)

instance Functor STree where
    fmap f (STree l n r) = STree (fmap (fmap f) l) (f n) (fmap (fmap f) r)

-- | Return the label value.
label :: STree a -> a
label (STree _ x _) = x
