module Woburn.Surface.Map
    ( SurfaceMap
    , lookupSurfaces
    , modifySurface
    , insertNew
    , delete
    , attach
    , shuffle
    , commit
    , setSync
    )
where

import Control.Arrow
import qualified Data.Map as M
import Data.STree
import qualified Data.STree.Zipper as Z
import Woburn.Surface
import qualified Woburn.Surface.Tree as ST

type SurfaceMap s = M.Map SurfaceId (Surface s, Either SurfaceId (STree SurfaceId))

-- | Maps 'SurfaceId' to the 'STree' it belongs in.
lookupSTree :: SurfaceId
            -> SurfaceMap s
            -> Maybe (STree SurfaceId)
lookupSTree sid ss = do
    (_, st) <- M.lookup sid ss
    findRoot st
    where
        findRoot (Right st) = return st
        findRoot (Left tid) = snd <$> M.lookup tid ss >>= findRoot

-- | Maps 'SurfaceId' to 'Surface'.
lookupSurface :: SurfaceId
              -> SurfaceMap s
              -> Maybe (Surface s)
lookupSurface sid = fmap fst . M.lookup sid

-- | Maps 'SurfaceId' to the 'STree' it belongs in, and maps 'SurfaceId's in
-- the tree to their corresponding 'Surface's.
lookupSurfaces :: SurfaceId
               -> SurfaceMap s
               -> Maybe (STree (Surface s))
lookupSurfaces sid sm = traverse (`lookupSurface` sm) =<< lookupSTree sid sm

-- | Modifies a surface.
modifySurface :: (Surface s -> Surface s)
              -> SurfaceId
              -> SurfaceMap s
              -> SurfaceMap s
modifySurface = M.adjust . first

-- | Inserts a shuffle for the given surface.
insertShuffle :: Shuffle
              -> SurfaceId
              -> SurfaceMap s
              -> SurfaceMap s
insertShuffle sh = M.adjust . first $ \s -> s { surfShuffle = sh : surfShuffle s }

-- | Inserts a new 'Surface' into the 'SurfaceMap'.
insertNew :: SurfaceId
          -> Surface s
          -> SurfaceMap s
          -> SurfaceMap s
insertNew sid surf = M.insert sid (surf, Right $ singleton sid)

-- | Updates the surface tree.
updateTree :: STree SurfaceId
           -> SurfaceMap s
           -> SurfaceMap s
updateTree st = M.adjust (second . const $ Right st) (label st)

-- | Updates the surface tree of all the children of a removed node.
updateChildren :: STree SurfaceId
               -> SurfaceMap s
               -> SurfaceMap s
updateChildren (STree ls _ rs) ss = foldr updateTree ss (ls ++ rs)

-- | Deletes a 'Surface' from the 'SurfaceMap'.
delete :: SurfaceId
       -> SurfaceMap s
       -> Maybe (SurfaceMap s)
delete sid ss = do
    stree <- lookupSTree sid ss
    ptr   <- ST.findSid sid stree

    let ops = M.delete sid :
                case ST.delete sid stree of
                  Nothing -> [ updateChildren stree ]
                  Just (stree', subtree, sh) ->
                      [ maybe id (insertShuffle sh . label . Z.getTree) (Z.up ptr)
                      , updateTree stree'
                      , updateChildren subtree
                      ]

    return $ foldr ($) ss ops

-- | Attaches a surface to another surface.
attach :: SurfaceId
       -> Maybe SurfaceId
       -> SurfaceMap s
       -> Maybe (SurfaceMap s)
attach sid tid ss = undefined

shuffle :: ShuffleOperation
        -> SurfaceId
        -> SurfaceId
        -> SurfaceMap s
        -> Maybe (SurfaceMap s)
shuffle op sid tid ss = undefined
        
commit :: SurfaceId
       -> SurfaceState
       -> SurfaceMap s
       -> Maybe ([Surface s], SurfaceMap s)
commit sid sm = undefined

setSync :: SurfaceId
        -> Bool
        -> SurfaceMap s
        -> Maybe ([Surface s], SurfaceMap s)
setSync sid sync sm = undefined
