{-# LANGUAGE FlexibleContexts #-}
module Woburn.Frontend.Subsurface
    ( subsurfaceSlots
    , makeSubsurfaceInert
    )
where

import Control.Monad.State
import Graphics.Wayland
import Linear
import qualified Woburn.Core as C
import Woburn.Frontend.Display.Object
import Woburn.Frontend.Types
import Woburn.Frontend.Types.Surface
import Woburn.Protocol


subsurfaceSlots :: SObject WlSurface -> SignalConstructor Server WlSubsurface Frontend
subsurfaceSlots surface subSurface = do
    modifySurface addSubsurface
    return
        WlSubsurfaceSlots { wlSubsurfaceDestroy     = destroy
                          , wlSubsurfaceSetPosition = setPosition
                          , wlSubsurfacePlaceAbove  = placeAbove
                          , wlSubsurfacePlaceBelow  = placeBelow
                          , wlSubsurfaceSetSync     = setSync True
                          , wlSubsurfaceSetDesync   = setSync False
                          }
    where
        surfaceId = surfaceToId surface

        modifySurface f = lift . modify $ \s -> s { fsSurfaces = adjustSurface f surface (fsSurfaces s) }

        addSubsurface sd = sd { sdSubsurface = Just subSurface }
        delSubsurface sd = sd { sdSubsurface = Nothing }

        destroy = do
            modifySurface delSubsurface
            sendRequest $ C.SurfaceAttach surfaceId Nothing
            destroyClientObject subSurface

        setPosition x y = sendRequest $ C.SurfaceSetPosition surfaceId (V2 x y)
        placeAbove sibling = sendRequest $ C.SurfacePlaceAbove surfaceId (surfaceToId sibling)
        placeBelow sibling = sendRequest $ C.SurfacePlaceBelow surfaceId (surfaceToId sibling)
        setSync = sendRequest . C.SurfaceSetSync surfaceId

inertSubsurfaceSlots :: SignalConstructor Server WlSubsurface Frontend
inertSubsurfaceSlots subsurface =
    return
        WlSubsurfaceSlots { wlSubsurfaceDestroy     = destroyClientObject subsurface
                          , wlSubsurfaceSetPosition = \_ _ -> return ()
                          , wlSubsurfacePlaceAbove  = \_ -> return ()
                          , wlSubsurfacePlaceBelow  = \_ -> return ()
                          , wlSubsurfaceSetSync     = return ()
                          , wlSubsurfaceSetDesync   = return ()
                          }

makeSubsurfaceInert :: SObject WlSubsurface -> Frontend ()
makeSubsurfaceInert obj = do
    slots <- inertSubsurfaceSlots obj
    unregisterObject obj
    registerObject obj slots
