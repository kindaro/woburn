module Woburn.Frontend.Types.Surface
    ( SurfaceData (..)
    , SurfacesData (..)
    , surfaceToId
    , nextSurfaceData
    , insertSurface
    , deleteSurface
    , adjustSurface
    , lookupSurface
    , insertCallbacks
    , lookupCallbacks
    , deleteCallbacks
    , initialSurfaceData
    , initialSurfacesData
    )
where

import Data.Int
import Data.Maybe
import qualified Data.Region as R
import qualified Data.Map as M
import Graphics.Wayland
import Linear
import Woburn.Buffer
import Woburn.Protocol.Core
import Woburn.Surface

-- | Convert from a 'WlSurface' object to its 'SurfaceId'.
surfaceToId :: SObject WlSurface -> SurfaceId
surfaceToId = fromIntegral . unObjId . unObject

-- | Per-surface data.
data SurfaceData =
    SurfaceData { sdDamageSurface   :: R.Region Int32
                , sdDamageBuffer    :: R.Region Int32
                , sdOpaque          :: R.Region Int32
                , sdInput           :: R.Region Int32
                , sdBuffer          :: Maybe Buffer
                , sdBufferOffset    :: V2 Int32
                , sdBufferTransform :: WlOutputTransform
                , sdBufferScale     :: Int32
                , sdFrameCallbacks  :: [SObject WlCallback]
                , sdSubsurface      :: Maybe (SObject WlSubsurface)
                }

-- | Data for all the surfaces.
data SurfacesData =
    SurfacesData { surfaces  :: M.Map (SObject WlSurface) SurfaceData
                 , callbacks :: M.Map SurfaceId [SObject WlCallback]
                 }

insertSurface :: SObject WlSurface -> SurfaceData -> SurfacesData -> SurfacesData
insertSurface surf dat sd = sd { surfaces = M.insert surf dat (surfaces sd) }

deleteSurface :: SObject WlSurface -> SurfacesData -> SurfacesData
deleteSurface surf sd = sd { surfaces = M.delete surf (surfaces sd) }

adjustSurface :: (SurfaceData -> SurfaceData) -> SObject WlSurface -> SurfacesData -> SurfacesData
adjustSurface f surf sd = sd { surfaces = M.adjust f surf (surfaces sd) }

lookupSurface :: SObject WlSurface -> SurfacesData -> Maybe SurfaceData
lookupSurface obj = M.lookup obj . surfaces

insertCallbacks :: SurfaceId -> [SObject WlCallback] -> SurfacesData -> SurfacesData
insertCallbacks sid cbs sd = sd { callbacks = M.insertWith (++) sid cbs (callbacks sd) }

lookupCallbacks :: SurfaceId -> SurfacesData -> [SObject WlCallback]
lookupCallbacks sid = fromMaybe [] . M.lookup sid . callbacks

deleteCallbacks :: SurfaceId -> SurfacesData -> SurfacesData
deleteCallbacks sid sd = sd { callbacks = M.delete sid (callbacks sd) }

-- | The initial surface state.
initialSurfacesData :: SurfacesData
initialSurfacesData =
    SurfacesData { surfaces  = M.empty
                 , callbacks = M.empty
                 }

nextSurfaceData :: SurfaceData -> SurfaceData
nextSurfaceData sd =
    sd { sdDamageSurface  = R.empty
       , sdDamageBuffer   = R.empty
       , sdBuffer         = Nothing
       , sdFrameCallbacks = []
       }

initialSurfaceData :: SurfaceData
initialSurfaceData =
    SurfaceData { sdDamageSurface   = R.empty
                , sdDamageBuffer    = R.empty
                , sdOpaque          = R.empty
                , sdInput           = R.everything
                , sdBuffer          = Nothing
                , sdBufferOffset    = 0
                , sdBufferTransform = WlOutputTransformNormal
                , sdBufferScale     = 1
                , sdFrameCallbacks  = []
                , sdSubsurface      = Nothing
                }
