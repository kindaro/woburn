module Woburn.Frontend.Subcompositor
    ( subcompositorSlots
    )
where

import Graphics.Wayland
import qualified Woburn.Core as C
import Woburn.Frontend.Display.Object
import Woburn.Frontend.Surface
import Woburn.Frontend.Subsurface
import Woburn.Frontend.Types
import Woburn.Protocol

subcompositorSlots :: SignalConstructor Server WlSubcompositor Frontend
subcompositorSlots subComp =
    return
        WlSubcompositorSlots { wlSubcompositorDestroy       = destroy
                             , wlSubcompositorGetSubsurface = getSubsurface
                             }
    where
        destroy = destroyClientObject subComp

        getSubsurface cons surface parent = do
            _ <- cons (subsurfaceSlots surface)
            sendRequest $ C.SurfaceAttach (surfaceToId surface) (Just $ surfaceToId parent)
