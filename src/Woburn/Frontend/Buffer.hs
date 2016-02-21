module Woburn.Frontend.Buffer
    ( releaseBuffer
    , acquireBuffer
    , bufferSlots
    )
where

import Control.Arrow
import Control.Monad.State
import Graphics.Wayland
import Woburn.Buffer
import Woburn.Frontend.Display.Object
import Woburn.Frontend.Types
import Woburn.Frontend.Types.Buffer as B
import Woburn.Protocol

stateB :: (BuffersData -> (a, BuffersData)) -> Frontend a
stateB f = lift . state $ \s -> second (\bd -> s { fsBuffers = bd }) (f (fsBuffers s))

-- | Decrements the reference count of a buffer by one, and signaling the
-- client if there are no more server references.
releaseBuffer :: Buffer -> Frontend ()
releaseBuffer buf = do
    val <- stateB $ B.release buf

    case val of
      Nothing       -> error "Trying to release a non-existing buffer"
      Just (1, obj) -> wlBufferRelease (signals obj)
      _             -> return ()

-- | Increments the reference count of a buffer by one.
acquireBuffer :: SObject WlBuffer -> Frontend Buffer
acquireBuffer obj = do
    mbuf <- stateB $ B.acquire obj
    case mbuf of
      Nothing  -> error "acquireBuffer called on a non-existing buffer object"
      Just buf -> return buf

bufferSlots :: Buffer -> SignalConstructor Server WlBuffer Frontend
bufferSlots buf bufObj = do
    lift . modify $ \s -> s { fsBuffers = B.insert bufObj buf (fsBuffers s) }
    return WlBufferSlots { wlBufferDestroy = destroy }
    where
        destroy = do
            lift . modify $ \s -> s { fsBuffers = B.delete bufObj (fsBuffers s) }
            destroyClientObject bufObj
