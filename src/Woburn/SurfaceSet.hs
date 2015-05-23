module Woburn.SurfaceSet
    ( SurfaceSet
    , SurfacePointer
    , findSid
    , inheritsSync
    , addShuffle
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
import Data.Traversable (traverse)
import Woburn.Surface

type SurfaceSet s = STree (Surface s)
type SurfacePointer s = Zipper (Surface s)

-- | Searches for a specific surface in a surface set and returns a zipper to it.
findSid :: SurfaceId -> SurfaceSet s -> Maybe (SurfacePointer s)
findSid sid = findFirst ((== sid) . surfId . label)

-- | Tries to find the common root of two surfaces that should be either
-- siblings or parent-child.
--
-- If they are siblings, the common root is their parent. If they are
-- parent-child, the parent is the common root.
findCommonRoot :: SurfaceId -> SurfaceId -> SurfaceSet s -> Maybe (SurfacePointer s)
findCommonRoot a b set = do
    guard (a /= b)

    za <- findSid a set
    zb <- findSid b set

    let da = depth za
        db = depth zb
        (goRoot, check) =
            case compare da db of
                 LT -> (goUp zb, guard . (za ==))
                 GT -> (goUp za, guard . (zb ==))
                 EQ -> (goUp zb, \p -> goUp za >>= guard . (p ==))

    root <- goRoot
    check root
    return root

-- | Adds a shuffle to the surface set.
--
-- It is assumed that the shuffle is valid, and that it should be inserted in
-- the root node.
addShuffle' :: Shuffle -> SurfaceSet s -> SurfaceSet s
addShuffle' sh (STree l n r) = STree l (n { surfShuffle = sh : surfShuffle n }) r

-- | Adds a shuffle to the surface set, or returns 'Nothing' if the shuffle is invalid.
addShuffle :: Shuffle -> SurfaceSet s -> Maybe (SurfaceSet s)
addShuffle sh@(Shuffle _ a b) = fmap (toTree . modify (addShuffle' sh)) . findCommonRoot a b

-- | Checks if the current node inherits the sync flag for any of its parents.
inheritsSync :: SurfacePointer s -> Bool
inheritsSync = any (surfSync . label . getTree) . parents

-- | Deletes a surface from the set, returning the set without the surface and
-- the subtree with the removed surface as the root.
--
-- If the surface isn't in the set, or it is the root of the entire set,
-- 'Nothing' is returned.
delete :: SurfaceId -> SurfaceSet s -> Maybe (SurfaceSet s, SurfaceSet s)
delete sid set = do
    ptr <- findSid sid set
    sh  <- createDeletedShuffle ptr
    first (toTree . modify (addShuffle' sh)) <$> Z.delete ptr

-- | Creates a 'DeletedAbove' or 'DeletedBelow' shuffle for the current tree.
createDeletedShuffle :: SurfacePointer s -> Maybe Shuffle
createDeletedShuffle ptr =
    (goLeft  ptr >>= createSiblingShuffle DeletedBelow)
    <|>
    (goRight ptr >>= createSiblingShuffle DeletedAbove)
    <|>
    (goUp    ptr >>= createParentShuffle)
    where
        createShuffle s x = Shuffle s (surfId . label $ getTree ptr) (surfId . label $ getTree x)

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

-- | Converts a 'SurfaceSet' to a list of 'SurfaceId's as well as a map from
-- 'SurfaceId' back to the subtree.
toList :: SurfaceSet s -> (M.Map SurfaceId (SurfaceSet s), Surface s, [SurfaceId])
toList set@(STree l n r) = (m, n, map (surfId . label) (l ++ [set] ++ r))
    where
        m = M.fromList (map (surfId . label &&& id) (l ++ r))

-- | Converts a list of 'SurfaceId's back to a 'SurfaceSet'
fromList :: M.Map SurfaceId (SurfaceSet s) -> Surface s -> [SurfaceId] -> Maybe (SurfaceSet s)
fromList m n ids =
    case span (/= surfId n) ids of
         (_   , []        ) -> Nothing
         (lids, _   : rids) ->
             STree
             <$> traverse (`M.lookup` m) lids
             <*> pure n
             <*> traverse (`M.lookup` m) rids

-- | Applies the shuffle in in root surface to the 'SurfaceSet'.
shuffle :: SurfaceSet s -> Maybe (SurfaceSet s)
shuffle set =
    fromList m (n { surfShuffle = [] }) . prune sh . shuffleList sh $ unprune sh ids
    where
        (m, n, ids) = toList set
        sh          = surfShuffle n
