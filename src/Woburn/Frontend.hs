{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Woburn.Frontend
    ( runFrontend
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
import Woburn.Frontend.WB

newtype SM a = SM { unSM :: ReaderT MessageLookup IO a }
    deriving (Functor, Applicative, Monad, MonadReader MessageLookup, MonadIO)

instance SocketError SM where
    sockErr = liftIO . ioError

instance SocketLookup SM where
    msgLookup = ask

-- | Runs an 'SM' computation.
runSM :: MessageLookup -> SM a -> IO a
runSM lut = (`runReaderT` lut) . unSM

-- | The global display object.
display :: Object Server WlDisplay
display = Object 1

-- | Handles an incoming message, sending a signal through the display object
-- if there is a protocol error.
handleMsg :: Message -> WB ()
handleMsg msg = do
    res <- (Nothing <$ dispatchMessage msg) `catchError` (return . Just)
    case res of
      Nothing                   -> return ()
      Just (WErrMethod obj err) -> wlDisplayError (signals display) obj WlDisplayErrorInvalidMethod err
      Just (WErrObject obj    ) -> wlDisplayError (signals display) obj WlDisplayErrorInvalidObject "unknown object"
      Just err                  -> throwError err

handleEvt :: C.Event -> WB ()
handleEvt = undefined

-- | Runs a single client.
runClient :: WB ()
runClient = do
    chan <- liftIO newChan
    sock <- ask
    rEvt <- getEvtChan

    ea <- liftIO . async . readUntilClosed rEvt $ writeChan chan . Right
    liftIO $ link ea

    forever $ do
        lut <- msgLookup
        sa  <- liftIO . async . runSM lut $ recv sock >>= liftIO . writeChan chan . Left
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
    _            <- forkIO . logError $ finally (runWB clientSocket rEvt wReq runClient) (close clientSocket)

    writeMChan wChan (wEvt, rReq)
    waitForClients socket wChan

-- | Starts the frontend, and returns a channel that new clients will be written to.
runFrontend :: Maybe String                                   -- ^ The path to listen for incoming connections on.
            -> IO (RMChan (WMChan C.Event, RMChan C.Request)) -- ^ A channel where new clients will be announced.
runFrontend path = do
    (rChan, wChan) <- newMChan
    bracket (listen path) (\sock -> async (waitForClients sock wChan) >>= link) close
    return rChan
