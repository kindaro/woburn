module Main
where

import Control.Concurrent
import Control.Concurrent.MChan.Split
import Data.Region
import Data.Word
import Foreign.ForeignPtr
import Foreign.Marshal
import Woburn.Backend.Gtk
import Woburn.Buffer
import Woburn.Core
import Woburn.Protocol
import Woburn.Surface

-- | Creates a new ARGB8 buffer where all the pixels have the same color.
createBuffer :: Int         -- ^ Width in pixels.
             -> Int         -- ^ Height in pixels.
             -> Word32      -- ^ Pixel color.
             -> IO Buffer   -- ^ The new buffer.
createBuffer w h color = do
    ptr  <- newArray $ replicate (w * h) color
    fptr <- newForeignPtr finalizerFree ptr
    return Buffer { bufData   = castForeignPtr fptr
                  , bufFormat = WlShmFormatArgb8888
                  , bufWidth  = fromIntegral w
                  , bufHeight = fromIntegral h
                  , bufStride = fromIntegral w * 4
                  }

createSurfaceState :: Word32 -> IO SurfaceState
createSurfaceState color = do
    buffer <- createBuffer 100 100 color
    return SurfaceState { surfBuffer       = Just buffer
                        , surfBufferOffset = 0
                        , surfBufferScale  = 1
                        , surfDamage       = everything
                        , surfOpaque       = everything
                        , surfInput        = everything
                        , surfTransform    = WlOutputTransformNormal
                        }

-- | A simple client that creates a window with three surfaces.
runClient :: RMChan Event -> WMChan Request -> IO ()
runClient evtRd reqWr = do
    surf1 <- createSurfaceState 0xffff0000
    surf2 <- createSurfaceState 0xff00ff00
    surf3 <- createSurfaceState 0xff0000ff

    writeMChan reqWr $ SurfaceCreate 0
    writeMChan reqWr $ SurfaceCreate 1
    writeMChan reqWr $ SurfaceCreate 2

    writeMChan reqWr $ SurfaceAttach 1 (Just 0)
    writeMChan reqWr $ SurfaceAttach 2 (Just 1)
    writeMChan reqWr $ SurfaceSetPosition 1 10
    writeMChan reqWr $ SurfaceSetPosition 2 10
    writeMChan reqWr $ SurfacePlaceBelow 2 1

    writeMChan reqWr $ SurfaceCommit 0 surf1
    writeMChan reqWr $ SurfaceCommit 1 surf2
    writeMChan reqWr $ SurfaceCommit 2 surf3
    writeMChan reqWr $ WindowCreate 0 0

    readUntilClosed evtRd $ \_ -> return ()
    closeMChan reqWr

main :: IO ()
main = do
    (clientRd, clientWr)    <- newMChan
    (reqWr, evtRd, surfGet) <- gtkBackend

    (coreEvtRd, coreEvtWr)  <- newMChan
    (coreReqRd, coreReqWr)  <- newMChan

    writeMChan clientWr (coreEvtWr, coreReqRd)
    closeMChan clientWr

    _ <- forkIO $ runClient coreEvtRd coreReqWr

    run reqWr evtRd surfGet clientRd
