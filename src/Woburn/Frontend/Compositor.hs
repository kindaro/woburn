module Woburn.Frontend.Compositor
    ( compositorSlots
    )
where

import Control.Monad
import Graphics.Wayland
import Woburn.Frontend.Region
import Woburn.Frontend.Surface
import Woburn.Frontend.Types
import Woburn.Protocol.Core

compositorSlots :: SignalConstructor Server WlCompositor Frontend
compositorSlots _ =
    return
        WlCompositorSlots { wlCompositorCreateSurface = createSurface
                          , wlCompositorCreateRegion  = createRegion
                          }
    where
        createSurface cons = void $ cons surfaceSlots
        createRegion cons = void $ cons regionSlots
