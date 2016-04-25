{-# LANGUAGE FlexibleContexts #-}
module Woburn.Frontend.Subsurface
    ( subsurfaceSlots
    , makeSubsurfaceInert
    )
where

import Control.Arrow
import Control.Monad.State
import Graphics.Wayland
import Linear
import Woburn.Frontend.Display.Object
import Woburn.Frontend.Types
import Woburn.Protocol.Core
import qualified Woburn.Core as C
import qualified Woburn.Frontend.Types.Surface as S

subsurfaceSlots :: SObject WlSurface -> SObject WlSurface -> SignalConstructor Server WlSubsurface Frontend
subsurfaceSlots surface parent subSurface = do
    modifySurfaces (S.addSubsurface surface subSurface parent)
    return
        WlSubsurfaceSlots { wlSubsurfaceDestroy     = destroy
                          , wlSubsurfaceSetPosition = setPosition
                          , wlSubsurfacePlaceAbove  = placeAbove
                          , wlSubsurfacePlaceBelow  = placeBelow
                          , wlSubsurfaceSetSync     = setSync True
                          , wlSubsurfaceSetDesync   = setSync False
                          }
    where
        modifySurfaces f = modify . second $ \s -> s { fsSurfaces = f (fsSurfaces s) }

        destroy = do
            modifySurfaces (S.delSubsurface surface parent)
            destroyClientObject subSurface

        setPosition x y = modifySurfaces (S.setPosition surface (V2 x y) parent)
        placeAbove sibling = modifySurfaces (S.placeAbove surface sibling parent)
        placeBelow sibling = modifySurfaces (S.placeBelow surface sibling parent)
        setSync sync = do
            surfs <- state $ \(o, s) ->
                second (\sd -> (o, s { fsSurfaces = sd }))
                . S.setSync surface sync $ fsSurfaces s
            sendRequest $ C.SurfaceCommit surfs

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
