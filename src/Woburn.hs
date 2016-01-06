{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}

module Woburn
    ( run
    )
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MChan.Split
import Control.Exception
import Control.Monad.Free.Church
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Data.Tuple
import qualified Data.Set.Diet as D
import Graphics.Wayland hiding (Event, Request)
import System.IO
import Woburn.Backend.Gtk
import Woburn.Core
import Woburn.Frontend
import Woburn.Frontend.Types

-- | Runs a single client.
runClient :: Socket -> RMChan Event -> WMChan Request -> IO ()
runClient sock rEvt wReq = do
    chan <- newChan
    ea   <- async . readUntilClosed rEvt $ writeChan chan . Right
    link ea

    forever . (`evalStateT` newObjectManager)  $ do
        mgr <- get
        sa  <- liftIO . async $ recv (messageLookup mgr) sock >>= writeChan chan . Left
        liftIO $ link sa
        m   <- liftIO $ readChan chan
        liftIO $ cancel sa

        let f = case m of
                  Left  msg -> handleMessage msg
                  Right evt -> handleEvent evt

        (res, mgr') <- liftIO $ foldF interpretFrontend (runFrontend f mgr)

        put mgr'
        liftIO $ logError res
    where
        interpretFrontend (SendMessage msg a) = send sock msg >> return a
        interpretFrontend (SendRequest req a) = writeMChan wReq req >> return a


-- | Logs any 'WError's to stderr.
logError :: Either ObjectError () -> IO ()
logError = either (hPrint stderr) return

-- | Waits for incoming client connections, and spins of new threads to run them.
waitForClients :: WMChan (WMChan Event, RMChan Request) -> Socket -> IO ()
waitForClients wChan socket = forever $ do
    clientSocket <- accept socket
    (rEvt, wEvt) <- newMChan
    (rReq, wReq) <- newMChan
    _            <- forkIO $ finally (runClient clientSocket rEvt wReq) (close clientSocket >> closeMChan wReq)

    writeMChan wChan (wEvt, rReq)

-- | Handles events and requests going to and from the different clients.
clientManager :: RMChan (WMChan Event, RMChan Request)
              -> WMChan (CoreInput s)
              -> Chan (Maybe ClientId, Event)
              -> IO ()
clientManager newClients coreInp clientEvts = do
    mvar <- newMVar . D.singletonI $ D.Interval minBound maxBound
    readUntilClosed newClients (newClient mvar)
    where
        newClient mvar (wEvt, rReq) = do
            cid       <- modifyMVar mvar (return . swap . fromMaybe (error "Ran out of client IDs") . D.minView)
            evtReader <- async $ do
                chan <- dupChan clientEvts
                forever $ do
                    (mCid, evt) <- readChan chan
                    when (maybe True (== cid) mCid) $ writeMChan wEvt evt

            reqWriter <- async $ do
                writeMChan coreInp (ClientAdd cid)
                readUntilClosed rReq  $ writeMChan coreInp . ClientRequest cid
                cancel evtReader
                closeMChan wEvt
                writeMChan coreInp (ClientDel cid)
                modifyMVar_ mvar (return . D.insert cid)

            link evtReader
            link reqWriter

-- | Starts Woburn.
--
-- Starts listening for new clients on the given path, or if the path is
-- 'Nothing' it default to the value of the "WAYLAND_DISPLAY" environment
-- variable. If that one does not exist it will use
-- "$XDG_RUNTIME_DIR/wayland-0" or "/tmp/wayland-0".
run :: Maybe String -> IO ()
run path = do
    clientEvts                <- newChan
    (inpRd, inpWr)            <- newMChan
    (clientRd, clientWr)      <- newMChan
    (bReqWr, bEvtRd, surfGet) <- gtkBackend

    async (bracket (listen path) close (waitForClients clientWr)) >>= link
    async (readUntilClosed bEvtRd (writeMChan inpWr . BackendEvent)) >>= link
    async (clientManager clientRd inpWr clientEvts) >>= link

    evalStateT (readUntilClosed inpRd (runCore surfGet bReqWr clientEvts)) newCoreState
    where
        runCore surfGet reqWr evtWr inp =
            StateT $ foldF (interpretCore surfGet reqWr evtWr) . runStateT (handleInput inp)

        interpretCore surfGet reqWr evtWr inp =
            case inp of
              ClientEvent cid evt a -> writeChan evtWr (cid, evt) >> return a
              BackendRequest  req a -> writeMChan reqWr req >> return a
              BackendSurfGet      f -> f <$> liftIO surfGet
              CoreError       err _ -> error err
