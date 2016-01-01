module Woburn.Frontend
    ( run
    )
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MChan.Split
import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Graphics.Wayland
import System.IO
import qualified Woburn.Core as C
import Woburn.Protocol
import Woburn.Frontend.Display
import Woburn.Frontend.Types

-- | Handles an incoming message, sending a signal through the display object
-- if there is a protocol error.
handleMsg :: Message -> Frontend ()
handleMsg msg = do
    res <- (Nothing <$ dispatchMessage msg) `catchError` (return . Just)
    case res of
      Nothing                   -> return ()
      Just (WErrMethod obj err) -> wlDisplayError (signals display) obj WlDisplayErrorInvalidMethod err
      Just (WErrObject obj    ) -> wlDisplayError (signals display) obj WlDisplayErrorInvalidObject "unknown object"
      Just err                  -> throwError err

handleEvt :: C.Event -> Frontend ()
handleEvt = undefined

-- | Runs a single client.
runClient :: Frontend ()
runClient = do
    chan <- liftIO newChan
    sock <- ask
    rEvt <- getEvtChan

    ea <- liftIO . async . readUntilClosed rEvt $ writeChan chan . Right
    liftIO $ link ea

    forever $ do
        sa <- asyncSM $ recv sock >>= liftIO . writeChan chan . Left
        liftIO $ link sa

        m <- liftIO $ readChan chan
        case m of
          Left  msg -> handleMsg msg
          Right evt -> handleEvt evt

        liftIO $ cancel sa

-- | Logs any 'WError's to stderr.
logError :: IO (Either WError ()) -> IO ()
logError m = m >>= either (hPrint stderr) return

-- | Waits for incoming client connections, and spins of new threads to run them.
waitForClients :: Socket -> WMChan (WMChan C.Event, RMChan C.Request) -> IO ()
waitForClients socket wChan = do
    clientSocket <- accept socket
    (rEvt, wEvt) <- newMChan
    (rReq, wReq) <- newMChan
    _            <- forkIO . logError $ finally (runFrontend clientSocket rEvt wReq runClient) (close clientSocket)

    writeMChan wChan (wEvt, rReq)
    waitForClients socket wChan

-- | Starts the frontend, and returns a channel that new clients will be written to.
run :: Maybe String                                   -- ^ The path to listen for incoming connections on.
    -> IO (RMChan (WMChan C.Event, RMChan C.Request)) -- ^ A channel where new clients will be announced.
run path = do
    (rChan, wChan) <- newMChan
    bracket (listen path) (\sock -> async (waitForClients sock wChan) >>= link) close
    return rChan
