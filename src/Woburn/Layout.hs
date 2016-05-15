module Woburn.Layout
    ( Layout
    , empty
    , fromUniverse
    , toList
    , difference
    )
where

import Control.Arrow
import Data.Monoid
import Data.Rect
import Data.Tuple
import Data.Word
import qualified Data.List.Zipper as Z
import qualified Data.Map as M
import qualified Data.Set as S
import Woburn.Output
import qualified Woburn.Universe as U

data Layout a =
    Layout { screens  :: [(OutputId, Rect Word32, [(Rect Word32, a)])]
           , floating :: [(Rect Word32, a)]
           }

instance Functor Layout where
    fmap f l = l { screens  = map (\(a, b, c) -> (a, b, map (fmap f) c)) (screens l)
                 , floating = map (fmap f) (floating l)
                 }

instance Foldable Layout where
    foldMap f l =
        foldMap (\(_, _, ws) -> foldMap (foldMap f) ws) (screens l)
        <> foldMap (foldMap f) (floating l)

instance Traversable Layout where
    traverse f l =
        Layout
        <$> traverse (\(a, b, c) -> (,,) a b <$> traverse (traverse f) c) (screens l)
        <*> traverse (traverse f) (floating l)

-- | Returns an empty 'Layout'.
empty :: Layout a
empty = Layout [] []

-- | Lays out a 'Universe'.
fromUniverse :: U.Universe a -> Layout a
fromUniverse u =
    Layout { screens  = map layoutScreen . Z.toList $ U.screens u
           , floating = map swap . M.toList $ U.floating u
           }
    where
        layoutScreen :: U.Screen a -> (OutputId, Rect Word32, [(Rect Word32, a)])
        layoutScreen s =
            let r   = mappedRect $ U.output s
                oid = U.outputId s
             in (oid, r, map ((,) r) . Z.toList . U.windows $ U.workspace s)

-- | Converts a 'Layout' to a list of 'OutputId's and the surfaces that are on that output.
toList :: Layout a -> [(OutputId, [(Rect Word32, a)])]
toList l =
    map
    (\(oid,rs,ws) ->
        (,) oid
        . map (first $ shift (- topLeft rs))
        $ filter (overlaps rs . fst) (floating l) ++ ws
    )
    (screens l)

-- | Returns a list of the windows that has changed between two 'Layout's.
difference :: (Eq a, Ord a) => Layout a -> Layout a -> [(Rect Word32, a)]
difference oldL newL = filter (`S.notMember` S.fromList old) new
    where
        old = floating oldL ++ concatMap (\(_, _, ws) -> ws) (screens oldL)
        new = floating newL ++ concatMap (\(_, _, ws) -> ws) (screens newL)
