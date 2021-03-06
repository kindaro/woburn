module Woburn.Surface.Map
    ( SurfaceMap
    , empty
    , lookup
    , lookupAll
    , lookupAllIds
    , insert
    , delete
    , adjust
    , elems
    )
where

import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import Data.Int
import Linear
import Prelude hiding (lookup)
import Woburn.Surface

type SurfaceMap s = M.Map SurfaceId (Surface s (V2 Int32, SurfaceId))

-- | An empty 'SurfaceMap'.
empty :: SurfaceMap s
empty = M.empty

-- | Maps 'SurfaceId' to 'Surface'.
lookup :: SurfaceId
       -> SurfaceMap s
       -> Maybe (Surface s (V2 Int32, SurfaceId))
lookup = M.lookup

-- | Inserts a new 'Surface' into the 'SurfaceMap'.
insert :: SurfaceId
       -> Surface s (V2 Int32, SurfaceId)
       -> SurfaceMap s
       -> SurfaceMap s
insert = M.insert

-- | Deletes a 'Surface' from the 'SurfaceMap'.
delete :: SurfaceId
       -> SurfaceMap s
       -> SurfaceMap s
delete = M.delete

-- | Returns a list of all the elements in the map.
elems :: SurfaceMap s -> [Surface s (V2 Int32, SurfaceId)]
elems = M.elems

-- | Looks up a surface and all its children, returning a list with the
-- surfaces sorted by Z-order along with their offset.
--
-- Returns 'Nothing' if one or more of the surfaces do not exist.
lookupAll :: V2 Int32       -- ^ A global offset to apply.
          -> SurfaceId      -- ^ The ID of the parent surface.
          -> SurfaceMap s   -- ^ The map to lookup in.
          -> [(V2 Int32, Surface s ())]
lookupAll globalOff sid sm =
    case fmap (\(off, tid) -> lookupAll (globalOff + off) tid sm) <$> lookup sid sm of
      Nothing   -> []
      Just surf ->
          let (l, r) = surfChildren $ surfState surf
          in (concat l ++ [(globalOff, modifyState (\s -> s { surfChildren = ([], []) }) surf)] ++ concat r)

-- | Looks up the 'SurfaceId' of a surface and all its children, sorted by
-- Z-order.
lookupAllIds :: SurfaceId -> SurfaceMap s -> [SurfaceId]
lookupAllIds sid sm =
    case fmap ((`lookupAllIds` sm) . snd) <$> lookup sid sm of
      Nothing   -> []
      Just surf ->
          let (l, r) = surfChildren $ surfState surf
          in (concat l ++ [sid] ++ concat r)

-- | Applies a function to a surface.
adjust :: (Surface s (V2 Int32, SurfaceId) -> Surface s (V2 Int32, SurfaceId))
       -> SurfaceId
       -> SurfaceMap s
       -> SurfaceMap s
adjust = M.adjust
