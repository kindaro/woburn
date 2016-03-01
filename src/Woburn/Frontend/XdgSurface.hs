module Woburn.Frontend.XdgSurface
    ( xdgSurfaceSlots
    , updateSize
    )
where

import Control.Arrow
import Control.Monad
import Control.Monad.State
import qualified Data.Set as S
import Data.Word
import Graphics.Wayland
import Linear
import Prelude hiding (lookup)
import Woburn.Frontend.Display.Object
import Woburn.Frontend.Types
import Woburn.Frontend.Types.Surface
import Woburn.Frontend.Types.Window
import Woburn.Protocol.Core
import Woburn.Protocol.XdgShell
import qualified Woburn.Core as C
import Woburn.Window

windowToId :: SObject XdgSurface -> WindowId
windowToId = fromIntegral . unObjId . unObject

modifyWindows :: (WindowsData -> WindowsData) -> Frontend ()
modifyWindows f = modify . second $ \s -> s { fsWindows = f (fsWindows s) }

sendConfigure :: SObject XdgSurface -> Frontend ()
sendConfigure surf = do
    (WindowData (V2 w h) states) <- gets $ lookup (windowToId surf) . fsWindows . snd
    serial                       <- nextEventSerial
    xdgSurfaceConfigure (signals surf) w h (map toWord32 $ S.toList states) serial

xdgSurfaceSlots :: SObject WlSurface -> SignalConstructor Server XdgSurface Frontend
xdgSurfaceSlots surface xdgSurface = do
    sendRequest . C.WindowCreate windowId $ surfaceToId surface
    return
        XdgSurfaceSlots { xdgSurfaceDestroy           = destroy
                        , xdgSurfaceSetParent         = setParent
                        , xdgSurfaceSetTitle          = setTitle
                        , xdgSurfaceSetAppId          = setAppId
                        , xdgSurfaceShowWindowMenu    = showMenu
                        , xdgSurfaceMove              = move
                        , xdgSurfaceResize            = resize
                        , xdgSurfaceAckConfigure      = ackConfigure
                        , xdgSurfaceSetWindowGeometry = setGeometry
                        , xdgSurfaceSetMaximized      = setMaximized
                        , xdgSurfaceUnsetMaximized    = unsetMaximized
                        , xdgSurfaceSetFullscreen     = setFullscreen
                        , xdgSurfaceUnsetFullscreen   = unsetFullscreen
                        , xdgSurfaceSetMinimized      = setMinimized
                        }
    where
        windowId = windowToId xdgSurface

        destroy = destroyClientObject xdgSurface
        setParent parent = return ()

        setTitle = sendRequest . C.WindowSetTitle windowId
        setAppId = sendRequest . C.WindowSetClass windowId

        showMenu seat serial x y = error "showMenu is not implemented yet"
        move _ _ = return ()
        resize _ _ _ = return ()
        ackConfigure _ = return ()
        setGeometry _ _ _ _ = return ()

        set st = do
            modifyWindows (setState st windowId)
            sendConfigure xdgSurface

        unset st = do
            modifyWindows (unsetState st windowId)
            sendConfigure xdgSurface

        setMaximized = set XdgSurfaceStateMaximized
        unsetMaximized = unset XdgSurfaceStateMaximized
        setFullscreen _ = set XdgSurfaceStateFullscreen
        unsetFullscreen = unset XdgSurfaceStateFullscreen

        setMinimized = return ()

updateSize :: WindowId -> V2 Word32 -> Frontend ()
updateSize windowId size = do
    modifyWindows (setSize size windowId)
    sendConfigure surf
    where
        surf = Object . ObjId $ fromIntegral windowId
