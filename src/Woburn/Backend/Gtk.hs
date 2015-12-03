module Woburn.Backend.Gtk
    ( gtkBackend
    )
where

import Control.Concurrent
import Control.Concurrent.MChan.Split
import Control.Exception
import Control.Monad.IO.Class
import Data.IORef
import Data.Int
import Data.Word
import GHC.Conc
import Graphics.UI.Gtk
import Linear
import Woburn.Buffer
import Woburn.Output
import Woburn.Protocol
import Woburn.Surface
import qualified Woburn.Backend as B

import Text.Printf

data GtkBuffer =
    GtkBuffer { buffer :: Buffer
              , offset :: V2 Int32
              , scale  :: Int
              , pixbuf :: Pixbuf
              }

newtype GtkSurface = GtkSurface { unGtkSurface :: IORef (Maybe GtkBuffer) }

mkOut :: Word32 -> Word32 -> Output
mkOut w h =
    Output { outputId          = 0
           , outputMake        = "Gtk window"
           , outputModel       = "Gtk window"
           , outputCurMode     = Mode { modeWidth = w, modeHeight = h, modeRefresh = 60000, modePreferred = True }
           , outputModes       = []
           , outputPhysWidth   = w * dpmm
           , outputPhysHeight  = h * dpmm
           , outputSubpixel    = WlOutputSubpixelUnknown
           , outputTransform   = WlOutputTransformNormal
           , outputScale       = 1
           }
    where
        -- Dots per millimeter
        dpmm = 10

createSurface :: IO GtkSurface
createSurface = GtkSurface <$> newIORef Nothing

pixbufFromBuffer :: Buffer -> IO Pixbuf
pixbufFromBuffer buf = withBuffer buf $ \ptr ->
    pixbufNewFromData
        ptr ColorspaceRgb True 8
        (fromIntegral $ bufWidth buf)
        (fromIntegral $ bufHeight buf)
        (fromIntegral $ bufStride buf)

gtkBufferFromState :: SurfaceState -> IO (Maybe GtkBuffer)
gtkBufferFromState s =
    case surfBuffer s of
      Nothing -> return Nothing
      Just b  -> Just . GtkBuffer b (surfBufferOffset s) (surfBufferScale s) <$> pixbufFromBuffer b

commitSurface :: Surface GtkSurface -> IO ()
commitSurface surf =
    case surfState surf of
      Nothing -> return ()
      Just s  -> writeIORef (unGtkSurface $ surfData surf) =<< gtkBufferFromState s

gtkBackend :: IO (WMChan (B.Request GtkSurface), RMChan B.Event, IO GtkSurface)
gtkBackend = do
    (evtRd, evtWr) <- newMChan
    (reqRd, reqWr) <- newMChan

    tid <- forkOS $ handle (print :: SomeException -> IO ()) (initGUI >> mainGUI)
    win <- postGUISync windowNew
    labelThread tid "GtkThread"
    postGUISync $ set win [windowTitle := "Woburn Compositor"]

    _   <- postGUISync . on win configureEvent $ do
        (w, h) <- eventSize
        liftIO $ printf "Window resized: (%i, %i)\n" w h
        liftIO . writeMChan evtWr . B.OutputAdded $ mkOut (fromIntegral w) (fromIntegral h)
        return True

    _ <- forkIO $ readUntilClosed reqRd reqHandler

    postGUISync $ widgetShowAll win
    return (reqWr, evtRd, createSurface)
    where
        reqHandler req =
            case req of
              B.OutputSetMode _ _               -> error "Gtk surfaces should have only one mode"
              B.SurfaceCommit surfaces layedOut ->
                  mapM_ commitSurface surfaces
