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
import Data.Time.Clock.POSIX
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
runClient :: FinalizerPtr Word8             -- ^ A finalizer for mmap'ed memory
          -> MVar (M.Map (Ptr Word8) CSize) -- ^ Data used by the mmap finalizer
          -> ClientId                       -- ^ The client ID of this client.
          -> Socket                         -- ^ The client socket.
          -> Chan (Either Message Event)    -- ^ The channel new events will arrive on.
          -> WMChan (CoreInput s)           -- ^ The channel core requests are sent on.
          -> IO ()
runClient munmapFinalizer finalizerData cid sock chan reqWr = do
    ((_, iMgr), iFs) <- foldF interpretFrontend (runFrontend initFrontend newObjectManager initialFrontendState)

    (`evalStateT` (iMgr, iFs)) . forever  $ do
        (mgr, fs) <- get
        sa        <- liftIO . async $ recv (messageLookup mgr) sock >>= writeChan chan . Left
        liftIO $ link sa

        m         <- liftIO $ readChan chan
        liftIO $ cancel sa
        liftIO $ putStrLn ("<< " ++ show m)

        let f = case m of
                  Left  msg -> handleMessage msg
                  Right evt -> handleEvent evt

        ((res, mgr'), fs') <- liftIO $ foldF interpretFrontend (runFrontend f mgr fs)

        put (mgr', fs')
        liftIO $ logError res
    where
        interpretFrontend (GetClientId f           ) = return $ f cid
        interpretFrontend (GetTimestamp f          ) = (f . round . (* 1000)) <$> getPOSIXTime

        interpretFrontend (SendMessage msg a       ) = do
            putStrLn (">> " ++ show msg)
            send sock msg
            return a

        interpretFrontend (SendRequest req a       ) = do
            putStrLn (">> " ++ show req)
            writeMChan reqWr (ClientRequest cid req)
            return a

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
waitForClients :: FinalizerPtr Word8                                  -- ^ A finalizer that calls munmap.
               -> MVar (M.Map (Ptr Word8) CSize)                      -- ^ Data used by the finalizer.
               -> MVar (D.Diet ClientId)                              -- ^ A set of free 'ClientId's
               -> MVar (M.Map ClientId (Chan (Either Message Event))) -- ^ A map of client's event channels.
               -> WMChan (CoreInput s)                                -- ^ The channel core requests are sent on.
               -> Socket                                              -- ^ The socket to listen on.
               -> IO ()
waitForClients munmapFinalizer finalizerData clientIds clientsVar wChan socket = forever $ do
    clientSocket <- accept socket
    cid          <- modifyMVar clientIds (return . swap . fromMaybe (error "Ran out of client IDs") . D.minView)
    evtChan      <- newChan

    modifyMVar_ clientsVar (return . M.insert cid evtChan)
    writeMChan wChan $ ClientAdd cid

    void .
        forkIO $
        runClient munmapFinalizer finalizerData cid clientSocket evtChan wChan
        `finally`
        finalizer cid clientSocket

    where
        finalizer cid sock = do
            close sock
            modifyMVar_ clientIds (return . D.insert cid)
            modifyMVar_ clientsVar (return . M.delete cid)
            writeMChan wChan $ ClientDel cid

-- | Dispatches events from core to the correct clients.
eventDispatcher :: MVar (M.Map ClientId (Chan (Either a Event)))
                -> Chan (Maybe ClientId, Event)
                -> IO ()
eventDispatcher clientsVar clientEvts = forever $ do
    (mCid, evt) <- readChan clientEvts
    withMVar clientsVar $ \clients ->
        case mCid of
          Nothing  -> mapM_ (`writeChan` Right evt) (M.elems clients)
          Just cid -> maybe (return ()) (`writeChan` Right evt) (M.lookup cid clients)

-- | Starts Woburn.
--
-- Starts listening for new clients on the given path, or if the path is
-- 'Nothing' it default to the value of the "WAYLAND_DISPLAY" environment
-- variable. If that one does not exist it will use
-- "$XDG_RUNTIME_DIR/wayland-0", or "/tmp/wayland-0" if $XDG_RUNTIME_DIR is not
-- set.
run :: Maybe String -> IO ()
run path = do
    clientEvts                 <- newChan
    (inpRd, inpWr)             <- newMChan
    (bReqWr, bEvtRd, surfGet)  <- gtkBackend
    clientIds                  <- newMVar . D.singletonI $ D.Interval minBound maxBound
    clientsVar                 <- newMVar M.empty
    (finalizer, finalizerData) <- makeMunmapFinalizer

    bracket (listen path) close $ \sock -> do
        wc <- async (waitForClients finalizer finalizerData clientIds clientsVar inpWr sock)
        be <- async (readUntilClosed bEvtRd (writeMChan inpWr . BackendEvent))
        ed <- async (eventDispatcher clientsVar clientEvts)
        mapM_ link [wc, be, ed]
        evalStateT (readUntilClosed inpRd (runCore surfGet bReqWr clientEvts)) newCoreState
        mapM_ cancel [wc, be, ed]
    where
        runCore surfGet reqWr evtWr inp =
            StateT $ foldF (interpretCore surfGet reqWr evtWr) . runStateT (handleInput inp)

        interpretCore surfGet reqWr evtWr inp =
            case inp of
              ClientEvent cid evt a -> writeChan evtWr (cid, evt) >> return a
              BackendRequest  req a -> writeMChan reqWr req >> return a
              BackendSurfGet      f -> f <$> surfGet
              CoreError       err a -> hPutStrLn stderr ("CoreError: " ++ err) >> return a
