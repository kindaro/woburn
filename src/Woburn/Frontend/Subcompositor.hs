module Woburn.Frontend.Subcompositor
    ( subcompositorSlots
    )
where

import Control.Monad
import Graphics.Wayland
import Woburn.Frontend.Display.Object
import Woburn.Frontend.Subsurface
import Woburn.Frontend.Types
import Woburn.Protocol.Core

subcompositorSlots :: SignalConstructor Server WlSubcompositor Frontend
subcompositorSlots subComp =
    return
        WlSubcompositorSlots { wlSubcompositorDestroy       = destroy
                             , wlSubcompositorGetSubsurface = getSubsurface
                             }
    where
        destroy = destroyClientObject subComp

        getSubsurface cons surface = void . cons . subsurfaceSlots surface
