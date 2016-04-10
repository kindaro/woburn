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
          -> Maybe [(V2 Int32, Surface s ())]
lookupAll globalOff sid sm = do
    surf <- lookup sid sm >>= traverse (\(off, tid) -> lookupAll (globalOff + off) tid sm)
    let (l, r) = surfChildren (surfState surf)
    return (concat l ++ [(globalOff, void surf)] ++ concat r)

-- | Looks up the 'SurfaceId' of a surface and all its children.
lookupAllIds :: SurfaceId -> SurfaceMap s -> Maybe [SurfaceId]
lookupAllIds sid sm = do
    surf <- lookup sid sm >>= traverse ((`lookupAllIds` sm) . snd)
    let (l, r) = surfChildren (surfState surf)
    return (concat l ++ [sid] ++ concat r)

adjust :: (Surface s (V2 Int32, SurfaceId) -> Surface s (V2 Int32, SurfaceId))
       -> SurfaceId
       -> SurfaceMap s
       -> SurfaceMap s
adjust = M.adjust
