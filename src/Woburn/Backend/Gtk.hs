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
import Data.Rect
import Data.Word
import GHC.Conc (labelThread)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
import Linear
import System.Mem.Weak
import Woburn.Buffer
import Woburn.Output
import Woburn.Protocol.Core
import Woburn.Surface
import qualified Woburn.Backend as B

data GtkBuffer =
    GtkBuffer { buffer :: Buffer
              , offset :: V2 Int32
              , scale  :: Int32
              , pixbuf :: Pixbuf
              }

data GtkSurface =
    GtkSurface { surfRef     :: IORef (Maybe GtkBuffer)
               , surfWeakRef :: Weak (IORef (Maybe GtkBuffer))
               }

finalizeGtkSurface :: WMChan B.Event -> IORef (Maybe GtkBuffer) -> IO ()
finalizeGtkSurface evtWr ref = do
    surf <- readIORef ref
    case surf of
      Nothing  -> return ()
      Just buf -> writeMChan evtWr . B.BufferReleased $ buffer buf

outId :: OutputId
outId = 0

mkOut :: Float -> Float -> Output
mkOut w h =
    Output { outputId          = outId
           , outputMake        = "Gtk window"
           , outputModel       = "Gtk window"
           , outputCurMode     = Mode { modeWidth = round w, modeHeight = round h, modeRefresh = 60000, modePreferred = True }
           , outputModes       = []
           , outputPhysWidth   = round $ w / dotsPerMm
           , outputPhysHeight  = round $ h / dotsPerMm
           , outputSubpixel    = WlOutputSubpixelUnknown
           , outputTransform   = WlOutputTransformNormal
           , outputScale       = 1
           }
    where
        dotsPerInch = 96
        dotsPerMm = dotsPerInch * mmPerInch
        mmPerInch = 25.4

createSurface :: WMChan B.Event -> IO GtkSurface
createSurface evtWr = do
    ref  <- newIORef Nothing
    weak <- mkWeakIORef ref (finalizeGtkSurface evtWr ref)
    return $ GtkSurface ref weak

pixbufFromBuffer :: Buffer -> IO Pixbuf
pixbufFromBuffer buf = withBuffer buf $ \ptr ->
    pixbufNewFromData
        ptr ColorspaceRgb True 8
        (fromIntegral $ bufWidth buf)
        (fromIntegral $ bufHeight buf)
        (fromIntegral $ bufStride buf)

gtkBufferFromState :: SurfaceState () -> IO (Maybe GtkBuffer)
gtkBufferFromState s =
    case surfBuffer s of
      Nothing -> return Nothing
      Just b  -> Just . GtkBuffer b (surfBufferOffset s) (surfBufferScale s) <$> pixbufFromBuffer b

commitSurface :: WMChan B.Event -> Surface GtkSurface () -> IO ()
commitSurface evtWr surf = do
    newBuf <- gtkBufferFromState $ surfState surf
    finalizeGtkSurface evtWr ref
    writeIORef ref newBuf
    where
        ref = surfRef $ surfData surf

drawSurface :: DrawWindow -> GC -> (V2 Int32, GtkSurface) -> IO ()
drawSurface dw gc (off, surf) = do
    mBuf <- readIORef $ surfRef surf
    case mBuf of
      Nothing  -> return ()
      Just buf -> do
          let V2 x y = fmap fromIntegral (off + offset buf)
          drawPixbuf dw gc (pixbuf buf) 0 0 x y (-1) (-1) RgbDitherNone 0 0
          touchBuffer (buffer buf)

drawWindow :: DrawWindow -> GC -> Rect Word32 -> [(V2 Int32, GtkSurface)] -> IO ()
drawWindow dw gc scissorRect surfaces  = do
    let r@(Rect (V2 x1 y1) _) = fmap fromIntegral scissorRect
    gcSetClipRectangle gc (Rectangle x1 y1 (width r) (height r))
    mapM_ (drawSurface dw gc) (reverse surfaces)

draw :: WMChan B.Event -> Window -> [(Rect Word32, [(V2 Int32, GtkSurface)])] -> IO ()
draw evtWr win windows = do
    dw <- widgetGetDrawWindow win
    gc <- gcNewWithValues dw defaultGCValues

    mapM_ (uncurry $ drawWindow dw gc) windows
    writeMChan evtWr $ B.OutputFrame outId
    where
        defaultGCValues =
            GCValues { foreground       = Color 0 0 0
                     , background       = Color 0 0 0
                     , function         = Copy
                     , fill             = Solid
                     , tile             = Nothing
                     , stipple          = Nothing
                     , clipMask         = Nothing
                     , subwindowMode    = ClipByChildren
                     , tsXOrigin        = 0
                     , tsYOrigin        = 0
                     , clipXOrigin      = 0
                     , clipYOrigin      = 0
                     , graphicsExposure = False
                     , lineWidth        = 1
                     , lineStyle        = LineSolid
                     , capStyle         = CapButt
                     , joinStyle        = JoinRound
                     }

gtkBackend :: IO (WMChan (B.Request GtkSurface), RMChan B.Event, IO GtkSurface)
gtkBackend = do
    (evtRd, evtWr) <- newMChan
    (reqRd, reqWr) <- newMChan

    tid       <- forkOS $ handle (print :: SomeException -> IO ()) (initGUI >> mainGUI)
    win       <- postGUISync windowNew
    layoutVar <- newMVar Nothing

    labelThread tid "GtkThread"
    postGUISync $ set win [windowTitle := "Woburn Compositor"]

    _ <- postGUISync . on win configureEvent $ do
        (w, h) <- eventSize
        liftIO . writeMChan evtWr . B.OutputAdded $ mkOut (fromIntegral w) (fromIntegral h)
        return True

    _ <- postGUISync . on win exposeEvent $ do
        liftIO . withMVar layoutVar $ maybe (return ()) (draw evtWr win)
        return True

    _ <- forkIO $ readUntilClosed reqRd (reqHandler evtWr win layoutVar)

    postGUISync $ widgetShowAll win
    return (reqWr, evtRd, createSurface evtWr)
    where
        reqHandler evtWr win layoutVar req =
            case req of
              B.OutputSetMode _ _               -> error "Gtk surfaces should have only one mode"
              B.SurfaceDestroy surfaces         -> mapM_ (finalize . surfWeakRef . surfData) surfaces
              B.SurfaceCommit surfaces layedOut -> do
                  threadsEnter
                  modifyMVar_ layoutVar $ \_ -> do
                      dw <- widgetGetDrawWindow win
                      w  <- drawWindowGetWidth dw
                      h  <- drawWindowGetHeight dw
                      mapM_ (commitSurface evtWr) surfaces
                      widgetQueueDrawArea win 0 0 w h
                      return (lookup outId layedOut)
                  threadsLeave
