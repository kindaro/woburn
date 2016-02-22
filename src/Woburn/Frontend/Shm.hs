module Woburn.Frontend.Shm
    ( shmSlots
    )
where

import Control.Monad
import Data.Int
import Data.Maybe
import Graphics.Wayland
import System.Posix.Types
import Woburn.Buffer
import Woburn.Frontend.Buffer
import Woburn.Frontend.Display.Object
import Woburn.Frontend.Types
import Woburn.Protocol.Core

shmPoolCons :: [WlShmFormat] -> Fd -> Int32 -> SignalConstructor Server WlShmPool Frontend
shmPoolCons fmts fd size pool = do
    ptr <- mapMemory fd size
    when (isNothing ptr) $ displayError pool WlShmErrorInvalidFd "Could not mmap fd"
    return WlShmPoolSlots { wlShmPoolCreateBuffer = createBuffer ptr
                          , wlShmPoolDestroy      = destroy
                          , wlShmPoolResize       = resize
                          }
    where
        checkDims offset width height stride =
            and [ size                   >  0
                , offset                 >= 0
                , width                  >  0
                , height                 >  0
                , stride                 >= width
                , maxBound `div` stride  >  height
                , size - height * stride >= offset
                ]

        createBuffer mptr bufferCons offset width height stride format =
            case (mptr, fromWord32 format, checkDims offset width height stride) of
              (Nothing , _       , _    ) -> displayError pool WlShmErrorInvalidFd "mmap failed"
              (_       , Nothing , _    ) -> displayError pool WlShmErrorInvalidFormat "Unknown format"
              (_       , _       , False) -> displayError pool WlShmErrorInvalidStride "Invalid width, height or stride"
              (Just ptr, Just fmt, _    )
                | fmt `notElem` fmts      -> displayError pool WlShmErrorInvalidFormat "Unsupported format"
                | otherwise               -> do
                  cid <- getClientId
                  let buf = Buffer { bufData     = ptr
                                   , bufFormat   = fmt
                                   , bufWidth    = fromIntegral width
                                   , bufHeight   = fromIntegral height
                                   , bufStride   = fromIntegral stride
                                   , bufOffset   = fromIntegral offset
                                   , bufClientId = cid
                                   }
                  void . bufferCons $ bufferSlots buf

        destroy = destroyClientObject pool

        resize newSize =
            if size >= newSize
              then displayError pool WlShmErrorInvalidFd "An Shm pool can not be made smaller"
              else do
                  newSlots <- shmPoolCons fmts fd newSize pool
                  unregisterObject pool
                  registerObject pool newSlots

shmSlots :: [WlShmFormat] -> SignalConstructor Server WlShm Frontend
shmSlots fmts shm = do
    mapM_ (wlShmFormat (signals shm)) fmts
    return WlShmSlots { wlShmCreatePool = createPool }
    where
        createPool cons fd size = void . cons $ shmPoolCons fmts fd size
