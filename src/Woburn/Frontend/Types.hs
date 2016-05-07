{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Woburn.Frontend.Types
    ( Frontend
    , FrontendState (..)
    , runFrontend
    , FrontendF (..)
    , unregisterObject

    , sendRequest
    , mapMemory
    , getClientId
    , getTimestamp
    , nextEventSerial
    , curEventSerial

    , initialFrontendState
    )
where

import Control.Arrow
import Control.Monad.Free.Church
import Control.Monad.Except
import Control.Monad.State
import Data.Int
import Data.Word
import Foreign.ForeignPtr
import Graphics.Wayland as G
import System.Posix.Types
import qualified Woburn.Core as C

import Woburn.Frontend.Types.Buffer
import Woburn.Frontend.Types.Global
import Woburn.Frontend.Types.Output
import Woburn.Frontend.Types.Region
import Woburn.Frontend.Types.Surface

import Woburn.Types

data FrontendF a =
    SendMessage Message (ObjectManager Server Frontend) a
  | SendRequest C.Request a
  | GetClientId (ClientId -> a)
  | GetTimestamp (Word32 -> a)
  | MapMemory Fd Int32 (Maybe (ForeignPtr Word8) -> a)
  deriving (Functor)

newtype Frontend a =
    Frontend { unFrontend :: ExceptT ObjectError (StateT (ObjectManager Server Frontend, FrontendState) (F FrontendF)) a }
    deriving
    ( Applicative
    , Functor
    , Monad
    , MonadError ObjectError
    , MonadFree FrontendF
    , MonadState (ObjectManager Server Frontend, FrontendState)
    )

instance MonadSend Frontend where
    sendMessage msg = do
        mgr <- gets fst
        liftF $ SendMessage msg mgr ()

instance MonadObject Server Frontend where
    allocObject = do
        mv <- gets $ alloc . fst
        case mv of
          Nothing       -> throwError $ ErrUser "No more free object IDs"
          Just (a, mgr) -> a <$ modify (first $ const mgr)

    registerObject obj slots = modify . first $ G.insert obj slots

    dispatchMessage msg = do
        handler <- gets $ lookupHandler (msgObj msg) . fst
        case handler of
          Nothing -> throwError . ErrObject $ msgObj msg
          Just h  -> h msg

    protocolError obj = throwError . ErrMethod obj

sendRequest :: C.Request -> Frontend ()
sendRequest req = liftF $ SendRequest req ()

getClientId :: Frontend ClientId
getClientId = liftF $ GetClientId id

getTimestamp :: Frontend Word32
getTimestamp = liftF $ GetTimestamp id

mapMemory :: Fd -> Int32 -> Frontend (Maybe (ForeignPtr Word8))
mapMemory fd size = liftF $ MapMemory fd size id

data FrontendState =
    FrontendState { fsGlobals     :: GlobalsData Frontend
                  , fsSurfaces    :: SurfacesData
                  , fsBuffers     :: BuffersData
                  , fsRegions     :: RegionsData
                  , fsOutputs     :: OutputsData
                  , fsEventSerial :: Word32
                  }

unregisterObject :: SObject i -> Frontend ()
unregisterObject = modify . first . G.delete

-- | Runs a 'Frontend' calculation.
runFrontend :: Frontend a
            -> ObjectManager Server Frontend
            -> FrontendState
            -> F FrontendF (Either ObjectError a, (ObjectManager Server Frontend, FrontendState))
runFrontend f mgr fs = runStateT (runExceptT $ unFrontend f) (mgr, fs)

nextEventSerial :: Frontend Word32
nextEventSerial = state $ \(o, s) -> (fsEventSerial s + 1, (o, s { fsEventSerial = fsEventSerial s + 1 }))

curEventSerial :: Frontend Word32
curEventSerial = gets $ fsEventSerial . snd

initialFrontendState :: FrontendState
initialFrontendState =
    FrontendState { fsGlobals     = initialGlobalsData
                  , fsSurfaces    = initialSurfacesData
                  , fsBuffers     = initialBuffersData
                  , fsRegions     = initialRegionsData
                  , fsOutputs     = initialOutputsData
                  , fsEventSerial = 0
                  }
