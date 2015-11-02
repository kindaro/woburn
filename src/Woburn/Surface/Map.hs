module Woburn.Surface.Map
    ( SurfaceMap
    , empty
    , lookupSTree
    , lookupSurfaces
    , modifySurface
    , insert
    , delete
    , attach
    , addShuffle
    , setState
    , setSync
    )
where

import Control.Applicative hiding (empty)
import Control.Arrow
import qualified Data.Map as M
import Data.Foldable
import Data.Maybe
import Data.STree
import qualified Data.STree.Zipper as Z
import Data.Traversable
import Prelude
import Woburn.Surface
import qualified Woburn.Surface.Tree as ST

type SurfaceMap s = M.Map SurfaceId (Surface s, Either SurfaceId (STree SurfaceId))

-- | An empty 'SurfaceMap'.
empty :: SurfaceMap s
empty = M.empty

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

-- | Maps 'SurfaceId' to the 'STree' it belongs in, and returns a pointer to
-- its position within the tree.
lookupZipper :: SurfaceId
             -> SurfaceMap s
             -> Maybe (Z.Zipper SurfaceId)
lookupZipper sid sm = lookupSTree sid sm >>= ST.findSid sid

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
insert :: SurfaceId
       -> Surface s
       -> SurfaceMap s
       -> SurfaceMap s
insert sid surf = M.insert sid (surf, Right $ singleton sid)

-- | Updates the surface tree.
updateTree :: STree SurfaceId
           -> SurfaceMap s
           -> SurfaceMap s
updateTree st = M.adjust (second . const $ Right st) (label st)

-- | Updates the surface tree of all the children of a removed node.
updateChildren :: STree SurfaceId
               -> SurfaceMap s
               -> SurfaceMap s
updateChildren (STree ls _ rs) ss = foldl' (flip updateTree) ss (ls ++ rs)

-- | Applies a list of operations on a surface map.
--
-- The operations are applied left-to-right.
applyOps :: [SurfaceMap s -> SurfaceMap s] -> SurfaceMap s -> SurfaceMap s
applyOps ops sm = foldl' (flip ($)) sm ops

-- | Detaches a surface from the surface it is currently attached, or does
-- nothing if it is not attached to another surface.
detach :: SurfaceId
       -> SurfaceMap s
       -> Maybe (SurfaceMap s)
detach sid ss = do
    stree <- lookupSTree sid ss
    ptr   <- ST.findSid sid stree
    return $ case ST.delete sid stree of
               Nothing                    -> ss
               Just (stree', subtree, sh) ->
                   applyOps
                   [ maybe id (insertShuffle sh . label . Z.getTree) (Z.up ptr)
                   , updateTree stree'
                   , updateTree subtree
                   ]
                   ss

-- | Deletes a 'Surface' from the 'SurfaceMap'.
delete :: SurfaceId
       -> SurfaceMap s
       -> Maybe (SurfaceMap s)
delete sid ss = do
    ss'   <- detach sid ss
    stree <- lookupSTree sid ss'
    return $ applyOps [ updateChildren stree, M.delete sid ] ss'

-- | Attaches a surface to another surface.
attach :: SurfaceId
       -> Maybe SurfaceId
       -> SurfaceMap s
       -> Maybe (SurfaceMap s)
attach sid mtid ss = do
    ss' <- detach sid ss
    case mtid of
      Nothing  -> return ss'
      Just tid -> do
          stree <- lookupSTree sid ss'
          ptr   <- lookupZipper tid ss'
          return $ updateTree (Z.toTree $ Z.insert stree ptr) ss'

-- | Adds a shuffle operation that will be executed at the next commit of the
-- common root.
addShuffle :: ShuffleOperation
           -> SurfaceId
           -> SurfaceId
           -> SurfaceMap s
           -> Maybe (SurfaceMap s)
addShuffle op sid tid ss = do
    let sh = Shuffle op sid tid
    stree <- lookupSTree sid ss
    root  <- ST.findCommonRoot sid tid stree
    return $ M.adjust (first $ \s -> s { surfShuffle = sh : surfShuffle s }) root ss

-- | Checks if a surface is in sync mode.
--
-- A surface is in sync mode if the sync flag is set in it, or any of its
-- ancestors.
inSyncMode :: SurfaceId
           -> SurfaceMap s
           -> Maybe Bool
inSyncMode sid sm = do
    idPtr  <- lookupZipper sid sm
    sPtr   <- fmap surfSync <$> traverse (`lookupSurface` sm) idPtr
    return . or $ map (label . Z.getTree) (sPtr : Z.parents sPtr)

-- | Commits a surface, and any sub-trees that are in sync mode.
--
-- When a surface is committed the current surface state is swapped with the
-- pending surface state, and the position of the sub-surfaces are updated.
commit :: SurfaceId
       -> SurfaceMap s
       -> Maybe ([Surface s], SurfaceMap s)
commit sid sm = do
    ptr  <- traverse (runKleisli $ Kleisli (`lookupSurface` sm) &&& returnA) =<< lookupZipper sid sm
    sync <- inSyncMode sid sm
    return . helper (not sync) ([], sm) $ Z.getTree ptr
    where
        helper checkSync (ss, sm') (STree ls (surf, tid) rs)
            | isNothing (surfState surf) = (ss, sm)
            | otherwise =
                let sm'' = applyOps ( modifySurface committed tid
                                    : map (modifySurface commitPosition . snd . label) (ls ++ rs)
                                    ) sm'
                    cs   = map (\(STree l n r) -> STree l (first commitPosition n) r) $
                            if checkSync
                              then filter (surfSync . fst . label) (ls ++ rs)
                              else ls ++ rs
                in
                foldl' (helper False) (surf : ss, sm'') cs

-- | Sets a new surface state for the specified surface.
--
-- If the surface is in sync mode, the new state will not be applied until its
-- parent surface's state is applied.
setState :: SurfaceId
         -> SurfaceState
         -> SurfaceMap s
         -> Maybe ([Surface s], SurfaceMap s)
setState sid st sm = do
    let sm' = modifySurface (\s -> s { surfState = Just st } ) sid sm
    sync <- inSyncMode sid sm'
    if sync
      then return ([], sm')
      else commit sid sm'

-- | Updates the synchronization mode on a surface.
--
-- If the surface is no longer in synchronized mode after the update, and it
-- has uncommitted state, it will be committed.
setSync :: SurfaceId
        -> Bool
        -> SurfaceMap s
        -> Maybe ([Surface s], SurfaceMap s)
setSync sid sync sm = do
    let sm' = modifySurface (\s -> s { surfSync = sync }) sid sm
    inSync <- inSyncMode sid sm'
    st     <- surfState <$> lookupSurface sid sm'
    if inSync || isNothing st
      then return ([], sm')
      else commit sid sm'
