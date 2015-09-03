{-|
Module      : Woburn.Surface.Tree
Description : Functions to work on trees of 'SurfaceId's
Copyright   : (C) Sivert Berg, 2015
License     : GPL3
Maintainer  : code@trev.is
Stability   : Experimental

Contains various functions to work on 'STree's of 'SurfaceId's.
|-}
module Woburn.Surface.Tree
    ( findSid
    , findCommonRoot
    , delete
    , shuffle
    )
where

import Control.Applicative
import Control.Arrow
import Control.Monad
import qualified Data.Map as M
import Data.STree
import Data.STree.Zipper hiding (delete)
import qualified Data.STree.Zipper as Z
import Woburn.Surface

-- | Searches for a specific surface in a surface set and returns a zipper to it.
findSid :: SurfaceId -> STree SurfaceId -> Maybe (Zipper SurfaceId)
findSid sid = findFirst ((== sid) . label)

-- | Tries to find the common root of two surfaces that should be either
-- siblings or parent-child.
--
-- If they are siblings, the common root is their parent. If they are
-- parent-child, the parent is the common root.
findCommonRoot :: SurfaceId -> SurfaceId -> STree SurfaceId -> Maybe SurfaceId
findCommonRoot a b set = do
    guard (a /= b)

    za <- findSid a set
    zb <- findSid b set

    let da = depth za
        db = depth zb
        (root, check) =
            case compare da db of
                 LT -> (up zb, guard . (za ==))
                 GT -> (up za, guard . (zb ==))
                 EQ -> (up zb, \p -> up za >>= guard . (p ==))

    rt <- root
    check rt
    return . label $ getTree rt

-- | Deletes a surface from the set, returning the set without the surface, the
-- subtree with the removed surface as the root and a delete shuffle.
--
-- If the surface isn't in the set, or it is the root of the entire set,
-- 'Nothing' is returned.
delete :: SurfaceId -> STree SurfaceId -> Maybe (STree SurfaceId, STree SurfaceId, Shuffle)
delete sid set = do
    ptr         <- findSid sid set
    sh          <- createDeletedShuffle ptr
    (ptr', del) <- Z.delete ptr
    return (toTree ptr', del, sh)

-- | Creates a 'DeletedAbove' or 'DeletedBelow' shuffle for the current tree.
createDeletedShuffle :: Zipper SurfaceId -> Maybe Shuffle
createDeletedShuffle ptr =
    (Z.left  ptr >>= createSiblingShuffle DeletedBelow)
    <|>
    (Z.right ptr >>= createSiblingShuffle DeletedAbove)
    <|>
    (Z.up    ptr >>= createParentShuffle)
    where
        createShuffle s x = Shuffle s (label $ getTree ptr) (label $ getTree x)

        createSiblingShuffle s x = createShuffle s x <$ guard (position ptr == position x)

        createParentShuffle x =
            case position ptr of
                 OnLeft  -> pure $ createShuffle DeletedAbove x
                 OnRight -> pure $ createShuffle DeletedBelow x
                 Root    -> Nothing

-- | Shuffles a list.
shuffleList :: [Shuffle] -> [SurfaceId] -> [SurfaceId]
shuffleList = flip (foldl f) . reverse
    where
        f []     _                  = []
        f (a:as) sh@(Shuffle s d x) =
            case (s, a == x, a == d) of
                 (DeletedAbove, _   , _   ) -> a : as
                 (DeletedBelow, _   , _   ) -> a : as
                 (PlaceAbove  , True, _   ) -> d : a : filter (/= d) as
                 (PlaceBelow  , True, _   ) -> a : d : filter (/= d) as
                 (_           , _   , True) -> f as sh
                 _                          -> a : f as sh

-- | Removes any deleted items from the list of surface IDs.
prune :: [Shuffle] -> [SurfaceId] -> [SurfaceId]
prune = flip $ foldl f
    where
        f []     _                  = []
        f (a:as) sh@(Shuffle s d _) =
            case (s, a == d) of
                 (PlaceAbove  , _   ) -> a : as
                 (PlaceBelow  , _   ) -> a : as
                 (_           , True) -> as
                 _                    -> a : f as sh

-- | Adds all the deleted ids back to a list of surface IDs.
unprune :: [Shuffle] -> [SurfaceId] -> [SurfaceId]
unprune = flip $ foldl f
    where
        f []     _                  = []
        f (a:as) sh@(Shuffle s d x) =
            case (s, a == x) of
                 (PlaceAbove  , _   ) -> a : as
                 (PlaceBelow  , _   ) -> a : as
                 (DeletedAbove, True) -> d : a : as
                 (DeletedBelow, True) -> a : d : as
                 _                    -> a : f as sh

-- | Converts a 'STree SurfaceId' to a list of 'SurfaceId's as well as a map
-- from 'SurfaceId' back to the subtree.
toList :: STree SurfaceId -> (M.Map SurfaceId (STree SurfaceId), SurfaceId, [SurfaceId])
toList set@(STree l n r) = (m, n, map label (l ++ [set] ++ r))
    where
        m = M.fromList (map (label &&& id) (l ++ r))

-- | Converts a list of 'SurfaceId's back to a 'STree SurfaceId'
fromList :: M.Map SurfaceId (STree SurfaceId) -> SurfaceId -> [SurfaceId] -> Maybe (STree SurfaceId)
fromList m n ids =
    case span (/= n) ids of
         (_   , []        ) -> Nothing
         (lids, _   : rids) ->
             STree
             <$> traverse (`M.lookup` m) lids
             <*> pure n
             <*> traverse (`M.lookup` m) rids

-- | Applies the shuffles to the 'STree SurfaceId'.
shuffle :: [Shuffle] -> STree SurfaceId -> Maybe (STree SurfaceId)
shuffle sh set =
    fromList m n . prune sh . shuffleList sh $ unprune sh ids
    where
        (m, n, ids) = toList set
