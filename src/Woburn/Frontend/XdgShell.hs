module Woburn.Frontend.XdgShell
    ( xdgShellSlots
    )
where

import Control.Monad
import Graphics.Wayland
import Woburn.Frontend.Display.Object
import Woburn.Frontend.Surface
import Woburn.Frontend.Types
import Woburn.Frontend.XdgSurface
import Woburn.Protocol.Core
import Woburn.Protocol.XdgShell

xdgShellSlots :: SignalConstructor Server XdgShell Frontend
xdgShellSlots shell =
    return
        XdgShellSlots { xdgShellDestroy            = destroy
                      , xdgShellUseUnstableVersion = useVersion
                      , xdgShellGetXdgSurface      = getSurface
                      , xdgShellGetXdgPopup        = getPopup
                      , xdgShellPong               = pong
                      }
    where
        destroy = destroyClientObject shell
        useVersion version =
            when (version /= toInt32 XdgShellVersionCurrent)
            $ displayError shell WlDisplayErrorInvalidMethod "incompatible xdg-shell versions"

        getSurface cons surf = void . cons $ xdgSurfaceSlots surf
        getPopup = error "XdgPopup not yet implemented"
        pong _ = return ()
