{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Woburn.Frontend.Types
    ( Frontend
    , runFrontend
    , sendRequest
    , getEvtChan
    , SM
    , asyncSM
    )
where

import Control.Concurrent.Async
import Control.Concurrent.MChan.Split
import Control.Monad.Except
import Control.Monad.Reader
import Graphics.Wayland
import qualified Woburn.Core as C

data FrontendData =
    FrontendData { evtChan :: RMChan C.Event
                 , reqChan :: WMChan C.Request
                 }

-- | The type of the frontend computations.
type Frontend = WS (ReaderT FrontendData IO)

-- | Sends a request to the core.
sendRequest :: C.Request -> Frontend ()
sendRequest req = do
    chan <- lift $ asks reqChan
    liftIO $ writeMChan chan req

-- | Gets the channel used to get core events.
getEvtChan :: Frontend (RMChan C.Event)
getEvtChan = lift $ asks evtChan

-- | Runs a 'Frontend' calculation.
runFrontend :: Socket -> RMChan C.Event -> WMChan C.Request -> Frontend a -> IO (Either WError a)
runFrontend s r w f = runReaderT (runW s f) (FrontendData r w)

-- | A simple monad used to run socket operations in a concurrent thread.
newtype SM a = SM { unSM :: ReaderT MessageLookup IO a }
    deriving (Functor, Applicative, Monad, MonadReader MessageLookup, MonadIO)

instance SocketError SM where
    sockErr = liftIO . ioError

instance SocketLookup SM where
    msgLookup = ask

-- | Starts an asynchronous routine that can be used to receive socket data.
asyncSM :: (MonadIO m, SocketLookup m) => SM () -> m (Async ())
asyncSM sm = do
    lut <- msgLookup
    liftIO . async $ runReaderT (unSM sm) lut
