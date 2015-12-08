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
import Data.STree
import Data.Word
import GHC.Conc
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC
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

mkOut :: Float -> Float -> Output
mkOut w h =
    Output { outputId          = 0
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

drawSurface :: DrawWindow -> GC -> (V2 Int32, GtkSurface) -> IO ()
drawSurface dw gc (off, surf) = do
    mBuf <- readIORef $ unGtkSurface surf
    case mBuf of
      Nothing  -> return ()
      Just buf -> do
          let V2 x y = fmap fromIntegral (off + offset buf)
          drawPixbuf dw gc (pixbuf buf) 0 0 x y (-1) (-1) RgbDitherNone 0 0
          touchBuffer (buffer buf)

drawWindow :: DrawWindow -> GC -> Rect Word32 -> STree (V2 Int32, GtkSurface) -> IO ()
drawWindow dw gc scissorRect surfaces  = do
    let r@(Rect (V2 x1 y1) _) = fmap fromIntegral scissorRect
    gcSetClipRectangle gc (Rectangle x1 y1 (width r) (height r))
    traverseR_ (drawSurface dw gc) surfaces
    where
        -- traverse_ that works right to left.
        traverseR_ :: (Foldable t, Applicative f) => (a -> f ()) -> t a -> f ()
        traverseR_ f = foldl (\b a -> f a *> b) (pure ())

draw :: Window -> [(Rect Word32, STree (V2 Int32, GtkSurface))] -> IO ()
draw win windows = do
    dw <- widgetGetDrawWindow win
    w  <- drawWindowGetWidth dw
    h  <- drawWindowGetHeight dw
    gc <- gcNewWithValues dw defaultGCValues

    drawWindowBeginPaintRect dw (Rectangle 0 0 w h)
    drawRectangle dw gc True 0 0 w h
    mapM_ (uncurry $ drawWindow dw gc) windows
    drawWindowEndPaint dw
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

    tid <- forkOS $ handle (print :: SomeException -> IO ()) (initGUI >> mainGUI)
    win <- postGUISync windowNew
    labelThread tid "GtkThread"
    postGUISync $ set win [windowTitle := "Woburn Compositor"]

    _   <- postGUISync . on win configureEvent $ do
        (w, h) <- eventSize
        liftIO $ printf "Window resized: (%i, %i)\n" w h
        liftIO . writeMChan evtWr . B.OutputAdded $ mkOut (fromIntegral w) (fromIntegral h)
        return True

    _ <- forkIO $ readUntilClosed reqRd (reqHandler win)

    postGUISync $ widgetShowAll win
    return (reqWr, evtRd, createSurface)
    where
        reqHandler win req =
            case req of
              B.OutputSetMode _ _               -> error "Gtk surfaces should have only one mode"
              B.SurfaceCommit surfaces layedOut -> postGUISync $ do
                      mapM_ commitSurface surfaces
                      maybe (return ()) (draw win) (lookup 0 layedOut)
