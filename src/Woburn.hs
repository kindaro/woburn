{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}

module Woburn
    ( run
    )
where

import Bindings.Posix.Sys.Mman
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
import Data.Word
import qualified Data.Map as M
import qualified Data.Set.Diet as D
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C.Types
import Graphics.Wayland hiding (Event, Request)
import System.IO
import System.Posix.Types
import Woburn.Backend.Gtk
import Woburn.Core
import Woburn.Frontend
import Woburn.Frontend.Types
import Woburn.Types

foreign import ccall"wrapper"
    wrapMunmapFinalizer :: (Ptr Word8 -> IO ()) -> IO (FinalizerPtr Word8)

-- | Creates a finalizer that calls munmap.
makeMunmapFinalizer :: IO (FinalizerPtr Word8, MVar (M.Map (Ptr Word8) CSize))
makeMunmapFinalizer = do
    mvar      <- newMVar M.empty
    finalizer <- wrapMunmapFinalizer (helper mvar)
    return (finalizer, mvar)
    where
        helper :: MVar (M.Map (Ptr Word8) CSize) -> Ptr Word8 -> IO ()
        helper mvar ptr = do
            msize <- withMVar mvar (return . M.lookup ptr)
            case msize of
              Nothing   -> error "Trying to finalize an mmaped ptr, but can't find its size"
              Just size -> void $ c'munmap (castPtr ptr) size

-- | Runs a single client.
runClient :: FinalizerPtr Word8
          -> MVar (M.Map (Ptr Word8) CSize)
          -> ClientId
          -> Socket
          -> RMChan Event
          -> WMChan Request
          -> IO ()
runClient munmapFinalizer finalizerData cid sock rEvt wReq = do
    chan             <- newChan
    ea               <- async . readUntilClosed rEvt $ writeChan chan . Right
    ((_, iMgr), iFs) <- foldF interpretFrontend (runFrontend initFrontend newObjectManager initialFrontendState)
    link ea

    forever . (`evalStateT` (iMgr, iFs))  $ do
        (mgr, fs) <- get
        sa        <- liftIO . async $ recv (messageLookup mgr) sock >>= writeChan chan . Left
        liftIO $ link sa

        m         <- liftIO $ readChan chan
        liftIO $ cancel sa

        let f = case m of
                  Left  msg -> handleMessage msg
                  Right evt -> handleEvent evt

        ((res, mgr'), fs') <- liftIO $ foldF interpretFrontend (runFrontend f mgr fs)

        put (mgr', fs')
        liftIO $ logError res
    where
        interpretFrontend (SendMessage msg a       ) = send sock msg >> return a
        interpretFrontend (SendRequest req a       ) = writeMChan wReq req >> return a
        interpretFrontend (GetClientId f           ) = return $ f cid
        interpretFrontend (MapMemory (Fd fd) size f) = do
            let cSize = fromIntegral size
            ptr <- c'mmap nullPtr cSize c'PROT_READ c'MAP_SHARED fd 0
            if ptr == c'MAP_FAILED
              then return $ f Nothing
              else do
                  let wordPtr = castPtr ptr
                  modifyMVar_ finalizerData (return . M.insert wordPtr cSize)
                  f . Just <$> newForeignPtr munmapFinalizer wordPtr

-- | Logs any 'WError's to stderr.
logError :: Either ObjectError () -> IO ()
logError = either (hPrint stderr) return

-- | Waits for incoming client connections, and spins of new threads to run them.
waitForClients :: FinalizerPtr Word8                              -- ^ A finalizer that calls munmap.
               -> MVar (M.Map (Ptr Word8) CSize)                  -- ^ Data used by the finalizer.
               -> MVar (D.Diet ClientId)                          -- ^ A set of free 'ClientId's
               -> WMChan (ClientId, WMChan Event, RMChan Request) -- ^ The channel new clients will be announced on.
               -> Socket                                          -- ^ The socket to listen on.
               -> IO ()
waitForClients munmapFinalizer finalizerData clientIds wChan socket = forever $ do
    clientSocket <- accept socket
    cid          <- modifyMVar clientIds (return . swap . fromMaybe (error "Ran out of client IDs") . D.minView)
    (rEvt, wEvt) <- newMChan
    (rReq, wReq) <- newMChan
    void .
        forkIO $
        finally
        (runClient munmapFinalizer finalizerData cid clientSocket rEvt wReq)
        (close clientSocket >> closeMChan wReq >> modifyMVar_ clientIds (return . D.insert cid))

    writeMChan wChan (cid, wEvt, rReq)

-- | Handles events and requests going to and from the different clients.
clientManager :: RMChan (ClientId, WMChan Event, RMChan Request)
              -> WMChan (CoreInput s)
              -> Chan (Maybe ClientId, Event)
              -> IO ()
clientManager newClients coreInp clientEvts =
    readUntilClosed newClients newClient
    where
        newClient (cid, wEvt, rReq) = do
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
    clientEvts                 <- newChan
    (inpRd, inpWr)             <- newMChan
    (clientRd, clientWr)       <- newMChan
    (bReqWr, bEvtRd, surfGet)  <- gtkBackend
    clientIds                  <- newMVar . D.singletonI $ D.Interval minBound maxBound
    (finalizer, finalizerData) <- makeMunmapFinalizer

    bracket (listen path) close $ \sock -> do
        wc <- async (waitForClients finalizer finalizerData clientIds clientWr sock)
        be <- async (readUntilClosed bEvtRd (writeMChan inpWr . BackendEvent))
        cm <- async (clientManager clientRd inpWr clientEvts)
        mapM_ link [wc, be, cm]
        evalStateT (readUntilClosed inpRd (runCore surfGet bReqWr clientEvts)) newCoreState
        mapM_ cancel [wc, be, cm]
    where
        runCore surfGet reqWr evtWr inp =
            StateT $ foldF (interpretCore surfGet reqWr evtWr) . runStateT (handleInput inp)

        interpretCore surfGet reqWr evtWr inp =
            case inp of
              ClientEvent cid evt a -> writeChan evtWr (cid, evt) >> return a
              BackendRequest  req a -> writeMChan reqWr req >> return a
              BackendSurfGet      f -> f <$> liftIO surfGet
              CoreError       err _ -> error err
