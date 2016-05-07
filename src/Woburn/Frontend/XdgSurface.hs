module Woburn.Frontend.XdgSurface
    ( xdgSurfaceSlots
    , updateSize
    )
where

import Control.Arrow
import Control.Monad
import Control.Monad.State
import qualified Data.Set as S
import Data.Rect
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
import Woburn.Surface

sendConfigure :: SObject WlSurface -> Frontend ()
sendConfigure surf = do
    mwd    <- gets $ lookupWindow surf . fsSurfaces . snd
    serial <- nextEventSerial
    case mwd of
      Nothing -> return ()
      Just wd ->
          let (V2 w h) = wdSize wd
           in xdgSurfaceConfigure
                (signals $ wdObject wd)
                w h
                (map toWord32 . S.toList $ wdStates wd)
                serial

modifySurfaces :: (SurfacesData -> SurfacesData) -> Frontend ()
modifySurfaces f = modify . second $ \s -> s { fsSurfaces = f (fsSurfaces s) }

modifyWindow :: (WindowData -> WindowData) -> SObject WlSurface -> Frontend ()
modifyWindow f surface = modifySurfaces (adjustWindow f surface)

xdgSurfaceSlots :: SObject WlSurface -> SignalConstructor Server XdgSurface Frontend
xdgSurfaceSlots surface xdgSurface = do
    modifySurfaces . insertWindow surface $ initialWindowData xdgSurface
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
        destroy = do
            modifySurfaces $ deleteWindow surface
            destroyClientObject xdgSurface

        setParent _ = return ()

        setTitle title = modifyWindow (\ws -> ws { wdTitle = title }) surface
        setAppId appId = modifyWindow (\ws -> ws { wdClass = appId }) surface

        showMenu seat serial x y = error "showMenu is not implemented yet"
        move _ _ = return ()
        resize _ _ _ = return ()
        ackConfigure _ = return ()
        setGeometry x y w h = modifyWindow (\ws -> ws { wdGeometry = Rect (V2 x y) (V2 (x + w - 1) (y + h - 1)) }) surface

        set st = do
            modifyWindow (setState st) surface
            sendConfigure surface

        unset st = do
            modifyWindow (unsetState st) surface
            sendConfigure surface

        setMaximized = set XdgSurfaceStateMaximized
        unsetMaximized = unset XdgSurfaceStateMaximized
        setFullscreen _ = set XdgSurfaceStateFullscreen
        unsetFullscreen = unset XdgSurfaceStateFullscreen

        setMinimized = return ()

updateSize :: SurfaceId -> V2 Word32 -> Frontend ()
updateSize sid sz = do
    modifyWindow (setSize sz) surf
    sendConfigure surf
    where
        surf = Object . ObjId $ fromIntegral sid
