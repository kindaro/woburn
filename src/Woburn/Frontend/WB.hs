module Woburn.Frontend.WB
    ( WB
    , runWB
    , sendRequest
    , getEvtChan
    )
where

import Control.Concurrent.MChan.Split
import Control.Monad.Reader
import Graphics.Wayland
import qualified Woburn.Core as C

data WBData =
    WBData { evtChan :: RMChan C.Event
           , reqChan :: WMChan C.Request
           }

type WB = WS (ReaderT WBData IO)

-- | Sends a request to the core.
sendRequest :: C.Request -> WB ()
sendRequest req = do
    chan <- lift $ asks reqChan
    liftIO $ writeMChan chan req

-- | Gets the channel used to get core events.
getEvtChan :: WB (RMChan C.Event)
getEvtChan = lift $ asks evtChan

-- | Runs a 'WB' calculation.
runWB :: Socket -> RMChan C.Event -> WMChan C.Request -> WB a -> IO (Either WError a)
runWB s r w wb = runReaderT (runW s wb) (WBData r w)
