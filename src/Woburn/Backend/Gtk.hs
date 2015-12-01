module Woburn.Backend.Gtk
    ( gtkBackend
    )
where

import Control.Concurrent
import Control.Concurrent.MChan.Split
import Control.Exception
import Control.Monad.IO.Class
import Data.IORef
import Data.Word
import GHC.Conc
import Graphics.UI.Gtk
import Woburn.Buffer
import Woburn.Output
import Woburn.Protocol
import qualified Woburn.Backend as B

import Text.Printf

data GtkBuffer =
    GtkBuffer { pixbuf :: Pixbuf
              , buffer :: Buffer
              }

newtype GtkSurface = GtkSurface (IORef GtkBuffer)

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

    _ <- forkIO $ reqHandler (0 :: Int) reqRd

    postGUISync $ widgetShowAll win
    return (reqWr, evtRd, undefined)
    where
        reqHandler n chan = do
            evt <- readMChan chan
            printf "req #%i came in\n" n
            reqHandler (n + 1) chan

