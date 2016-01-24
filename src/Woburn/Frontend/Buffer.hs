module Woburn.Frontend.Buffer
    ( releaseBuffer
    , acquireBuffer
    , bufferSlots
    )
where

import Control.Arrow
import Control.Monad.State
import qualified Data.Map as M
import Graphics.Wayland
import Woburn.Buffer
import Woburn.Frontend.Types
import Woburn.Protocol

-- | Decrements the reference count of a buffer by one, and signaling the
-- client if there are no more server references.
releaseBuffer :: Buffer -> Frontend ()
releaseBuffer buf = do
    val <- lift . state $ \s -> second (\m -> s { bufRefCounts = m }) (decrRefCount $ bufRefCounts s)
    case val of
      Nothing       -> error "Trying to release a non-existing buffer"
      Just (1, obj) -> wlBufferRelease (signals obj)
      _             -> return ()
    where
        decrRefCount = M.updateLookupWithKey updateEntry buf 
        updateEntry _ (n, o)
            | n == 1    = Nothing
            | n <  1    = error "Ref count is less than 1, this should never happen"
            | otherwise = Just (n - 1, o)

-- | Increments the reference count of a buffer by one.
acquireBuffer :: SObject WlBuffer -> Frontend Buffer
acquireBuffer obj = do
    mbuf <- M.lookup obj <$> lift (gets buffers)
    case mbuf of
      Nothing  -> error "acquireBuffer called on a non-existing buffer object"
      Just buf -> do
          lift . modify $ \s -> s { bufRefCounts = M.alter alterEntry buf (bufRefCounts s) }
          return buf
    where
        alterEntry Nothing       = Just (1, obj)
        alterEntry (Just (n, o)) = Just (n + 1, o)

bufferSlots :: Buffer -> SignalConstructor Server WlBuffer Frontend
bufferSlots buf bufObj = do
    lift . modify $ \s -> s { buffers = M.insert bufObj buf (buffers s) }
    return WlBufferSlots { wlBufferDestroy = destroy }
    where
        destroy = lift . modify $ \s -> s { buffers = M.delete bufObj (buffers s) }
