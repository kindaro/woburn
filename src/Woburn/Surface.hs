{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Woburn.Surface
    ( SurfaceMonad (..)
    , SurfaceId
    , Surface (..)
    , SurfaceState (..)
    , SurfaceSet
    , newSurface
    , commit
    , setSync
    , placeAbove
    , placeBelow
    , delete
    )
where

import Control.Applicative hiding (empty)
import Control.Monad
import Data.Int
import Data.Function
import Data.Ord
import Data.Region
import Data.STree
import Data.STree.Zipper
import Data.Word
import Linear
import Graphics.Wayland
import Woburn.Protocol

class Monad m => SurfaceMonad m s | m -> s where
    surfCommit :: Surface s -> m ()
    surfCreate :: m s

data Buffer = Buffer

data Role =
    SubSurface
  | ShellSurface
  | Cursor

data Buffered a = Buffered { currentState :: !a, pendingState :: !a }

type SurfaceId = Word32

data Shuffle =
    PlaceBelow SurfaceId SurfaceId
  | PlaceAbove SurfaceId SurfaceId

data SurfaceState =
    SurfaceState { surfBuffer       :: Maybe Buffer
                 , surfBufferOffset :: V2 Int
                 , surfBufferScale  :: Int
                 , surfDamage       :: Region Int32
                 , surfOpaque       :: Region Int32
                 , surfInput        :: Region Int32
                 , surfTransform    :: WlOutputTransform
                 }

data Surface s =
    Surface { surfId            :: SurfaceId             -- ^ Unique ID.
            , surfSync          :: Bool                  -- ^ Whether the surface is in sync mode.
            , surfDeleted       :: Bool                  -- ^ Whether the surface has been deleted.
            , surfNewState      :: Bool                  -- ^ Whether a new state has been committed.
            , surfState         :: Buffered SurfaceState -- ^ Current and pending surface state.
            , surfPosition      :: Buffered (V2 Int)     -- ^ Surface position relative to parent surface.
            , surfFrameCallback :: [SObject WlCallback]  -- ^ Called when a new frame should be drawn.
            , surfRole          :: Maybe Role            -- ^ The surface role.
            , surfShuffle       :: [Shuffle]             -- ^ Shuffle operations to perform on the next commit.
            , surfCurInput      :: Region Int32          -- ^ Current input region. If this surface is in sync mode
                                                         --   this is copied from the current state when the parent
                                                         --   is committed, otherwise it is updated at the same time
                                                         --   as the current state.
            , surfBackendData   :: s                     -- ^ Internal data used by the backend.
            }

instance Eq (Surface s) where
    (==) = (==) `on` surfId

instance Ord (Surface s) where
    compare = comparing surfId

type SurfaceSet s = STree (Surface s)

-- | The initial state given to a new surface.
initialState :: SurfaceState
initialState =
    SurfaceState { surfBuffer       = Nothing
                 , surfBufferOffset = V2 0 0
                 , surfBufferScale  = 1
                 , surfDamage       = empty
                 , surfOpaque       = empty
                 , surfInput        = everything
                 , surfTransform    = WlOutputTransformNormal
                 }

-- | Calculates the new pending state when the pending state is committed.
nextState :: SurfaceState -> SurfaceState
nextState state =
    state { surfBuffer       = Nothing
          , surfBufferOffset = V2 0 0
          , surfDamage       = empty
          }

-- | Updates the current value with the pending value, and calculates a new pending value.
flipBuffered :: (a -> a) -> Buffered a -> Buffered a
flipBuffered f b = Buffered { currentState = pendingState b
                            , pendingState = f (pendingState b)
                            }

-- | Creates a new surface.
newSurface :: SurfaceMonad m s => m (Surface s)
newSurface = do
    s <- surfCreate
    return Surface { surfId            = undefined
                   , surfSync          = False
                   , surfDeleted       = False
                   , surfNewState      = False
                   , surfState         = Buffered initialState initialState
                   , surfPosition      = Buffered 0 0
                   , surfFrameCallback = []
                   , surfRole          = Nothing
                   , surfShuffle       = []
                   , surfCurInput      = everything
                   , surfBackendData   = s
                   }

-- | Commits the pending state to the current state.
commitState :: Surface s -> Surface s
commitState surf = surf { surfState    = flipBuffered nextState (surfState surf)
                        , surfNewState = True
                        }

-- | Commits the surface to the backend, and updates surface position and input
-- region.
commitBackend :: SurfaceMonad m s => Surface s -> m (Surface s)
commitBackend surf
    | not (surfNewState surf) = return surf
    | otherwise               = do
        let surf' = surf { surfCurInput = surfInput . currentState $ surfState surf
                         , surfPosition = flipBuffered id $ surfPosition surf
                         , surfNewState = False
                         }
        surfCommit surf'
        return surf'

-- | Applies all the shuffles in 'surfShuffle'.
shuffle :: SurfaceSet s -> SurfaceSet s
shuffle = id

-- | Prunes the subtree of any child-surfaces that has been deleted.
prune :: SurfaceSet s -> SurfaceSet s
prune = id

-- | Modifies a SurfceSet by applying functions to its branches and node.
modifySet :: (Monad m, Applicative m)
          => (SurfaceSet s -> m (SurfaceSet s))
          -> (Surface s -> m (Surface s))
          -> SurfaceSet s
          -> m (SurfaceSet s)
modifySet branchFunc nodeFunc (STree l n r) =
    STree <$> mapM branchFunc l <*> nodeFunc n <*> mapM branchFunc r

-- | Commits a single surface.
commitSingle :: SurfaceMonad m s
             => Bool          -- ^ Whether the surface state should be updated.
             -> Bool          -- ^ Whether the backend should be updated.
             -> Surface s     -- ^ The surface to update.
             -> m (Surface s) -- ^ A computation returning the new surface
commitSingle updateSurface updateBackend surf =
    let f = if updateSurface then commitState   else id
        g = if updateBackend then commitBackend else return
    in  g (f surf)

-- | Commits a child sub-tree.
--
-- Only subtrees where the root surface has the sync-flag set is committed.
commitChildren :: (SurfaceMonad m s, Applicative m) => Bool -> SurfaceSet s -> m (SurfaceSet s)
commitChildren root set
    | root && not (surfSync (label set)) = pure set
    | otherwise                          = prune . shuffle <$> modifySet (commitChildren False) (commitSingle False True) set

-- | Commits a subtree of surfaces.
--
-- If the root of the subtree is in sync-mode, only the root surface is
-- updated. If it is desync-mode, subtrees where the root-surface has the
-- sync-flag set is committed as well.
commitRoot :: (SurfaceMonad m s, Applicative m) => Bool -> SurfaceSet s -> m (SurfaceSet s)
commitRoot sync' set =
    f <$> modifySet c (commitSingle True $ not sync) set
    where
        sync = sync' || surfSync (label set)
        (c, f) = if sync
                   then (pure               , id             )
                   else (commitChildren True, prune . shuffle)

-- | Commits all subsurfaces in a subtree until it hits a subsurface with the
-- sync-flag set.
commitSync :: (SurfaceMonad m s, Applicative m) => SurfaceSet s -> m (SurfaceSet s)
commitSync set
    | surfSync (label set) = pure set
    | otherwise            = modifySet commitSync (commitSingle False True) set

-- | Checks if the current node inherits the sync flag for any of its parents.
inheritsSync :: Zipper (Surface s) -> Bool
inheritsSync = any (surfSync . label . getTree) . parents

-- | Searches for a specific surface in a surface set and returns a zipper to it.
findSid :: SurfaceId -> SurfaceSet s -> Maybe (Zipper (Surface s))
findSid sid = findFirst ((\s -> surfId s == sid && not (surfDeleted s)) . label)

-- | Searches for a specific surface in surface set, then applies a function to
-- the subtree it is the parent of.
modifySid :: (Applicative m, Monad m)
          => (Bool -> SurfaceSet s -> m (SurfaceSet s))
          -> SurfaceId -> SurfaceSet s -> m (SurfaceSet s)
modifySid func sid set =
    case findFirst ((== sid ) . surfId . label) set of
         Nothing -> pure set
         Just z  -> toTree <$> modifyA (func $ inheritsSync z) z

-- | Commits a surface, and possibly its subsurfaces.
commit :: (SurfaceMonad m s, Applicative m) => SurfaceId -> SurfaceSet s -> m (SurfaceSet s)
commit = modifySid commitRoot

-- | Updates the sync value of a surface.
--
-- If the surface is the top-most surface with the sync-flag set, and the flag
-- is disabled, all surface below it without the sync-flag set must be
-- committed.
setSync :: (SurfaceMonad m s, Applicative m) => Bool -> SurfaceId -> SurfaceSet s -> m (SurfaceSet s)
setSync newVal = modifySid updateSync
    where
        updateSync sync curSet@(STree l n r)
            | curVal == newVal = pure curSet
            | sync   || newVal = pure newSet
            | otherwise        = modifySet commitSync pure newSet
            where
                n'     = n { surfSync = newVal }
                newSet = STree l n' r
                curVal = surfSync n

-- | Tries to find the common root of two surfaces that should be either
-- siblings or parent-child.
--
-- If they are siblings, the common root is their parent. If they are
-- parent-child, the parent is the common root.
findCommonRoot :: SurfaceId -> SurfaceId -> SurfaceSet s -> Maybe (Zipper (Surface s))
findCommonRoot a b set = do
    guard (a /= b)

    za <- findSid a set
    zb <- findSid b set

    let da = depth za
        db = depth zb
        (goRoot, check)
            | da <  db = (goUp zb, guard . (za ==))
            | da >  db = (goUp za, guard . (zb ==))
            | da == db = (goUp zb, \p -> goUp za >>= guard . (p ==))

    root <- goRoot
    check root
    return root

-- | Adds a shuffle command to the root of a surface set.
addShuffle :: Shuffle -> SurfaceSet s -> SurfaceSet s
addShuffle s (STree l n r) = STree l (n { surfShuffle = s : surfShuffle n}) r

-- | Places a surface above another surface.
placeAbove :: SurfaceId -> SurfaceId -> SurfaceSet s -> Maybe (SurfaceSet s)
placeAbove a b = fmap (toTree . modify (addShuffle $ PlaceAbove a b)) . findCommonRoot a b

-- | Places a surface below another surface.
placeBelow :: SurfaceId -> SurfaceId -> SurfaceSet s -> Maybe (SurfaceSet s)
placeBelow a b = fmap (toTree . modify (addShuffle $ PlaceBelow a b)) . findCommonRoot a b

-- | Removes a surface from a surface set.
delete :: SurfaceId -> SurfaceSet s -> Maybe (SurfaceSet s)
delete sid = fmap (toTree . modify f) . findSid sid
    where
        f (STree l n r) = STree l (n { surfDeleted = True }) r
