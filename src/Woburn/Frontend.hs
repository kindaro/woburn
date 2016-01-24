module Woburn.Frontend
    ( handleMessage
    , handleEvent
    , initFrontend
    )
where

import Control.Monad.Except
import Graphics.Wayland
import qualified Woburn.Core as C
import Woburn.Protocol
import Woburn.Frontend.Buffer
import Woburn.Frontend.Compositor
import Woburn.Frontend.Display
import Woburn.Frontend.Registry
import Woburn.Frontend.Shm
import Woburn.Frontend.Types

-- | Handles an incoming message, sending a signal through the display object
-- if there is a protocol error.
handleMessage :: Message -> Frontend ()
handleMessage msg = do
    res <- (Nothing <$ dispatchMessage msg) `catchError` (return . Just)
    case res of
      Nothing                  -> return ()
      Just (ErrMethod obj err) -> wlDisplayError (signals display) obj WlDisplayErrorInvalidMethod err
      Just (ErrObject obj    ) -> wlDisplayError (signals display) obj WlDisplayErrorInvalidObject "unknown object"
      Just err                 -> throwError err

-- | Handles an incoming core event.
handleEvent :: C.Event -> Frontend ()
handleEvent evt =
    case evt of
      C.BufferReleased buf -> releaseBuffer buf

initFrontend :: Frontend ()
initFrontend = do
    registerObject display displaySlots
    void $ addGlobal compositorSlots
