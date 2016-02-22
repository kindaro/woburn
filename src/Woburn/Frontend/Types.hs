{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Woburn.Frontend.Types
    ( Frontend
    , FrontendState (..)
    , runFrontend
    , FrontendF (..)

    , sendRequest
    , mapMemory
    , getClientId
    , getTimestamp
    , nextEventSerial
    , curEventSerial

    , initialFrontendState
    )
where

import Control.Monad.Free.Church
import Control.Monad.State
import Data.Int
import Data.Word
import Foreign.ForeignPtr
import Graphics.Wayland
import System.Posix.Types
import qualified Woburn.Core as C

import Woburn.Frontend.Types.Buffer
import Woburn.Frontend.Types.Global
import Woburn.Frontend.Types.Output
import Woburn.Frontend.Types.Region
import Woburn.Frontend.Types.Surface
import Woburn.Frontend.Types.Window

import Woburn.Types

data FrontendF a =
    SendMessage Message a
  | SendRequest C.Request a
  | GetClientId (ClientId -> a)
  | GetTimestamp (Word32 -> a)
  | MapMemory Fd Int32 (Maybe (ForeignPtr Word8) -> a)
  deriving (Functor)

newtype Inner a = Inner { unInner :: StateT FrontendState (F FrontendF) a }
    deriving (Applicative, Functor, Monad, MonadFree FrontendF, MonadState FrontendState)

instance MonadSend Inner where
    sendMessage msg = liftF $ SendMessage msg ()

sendRequest :: C.Request -> Frontend ()
sendRequest req = lift . liftF $ SendRequest req ()

getClientId :: Frontend ClientId
getClientId = lift . liftF $ GetClientId id

getTimestamp :: Frontend Word32
getTimestamp = lift . liftF $ GetTimestamp id

mapMemory :: Fd -> Int32 -> Frontend (Maybe (ForeignPtr Word8))
mapMemory fd size = lift . liftF $ MapMemory fd size id

data FrontendState =
    FrontendState { fsGlobals     :: GlobalsData Frontend
                  , fsSurfaces    :: SurfacesData
                  , fsBuffers     :: BuffersData
                  , fsRegions     :: RegionsData
                  , fsOutputs     :: OutputsData
                  , fsWindows     :: WindowsData
                  , fsEventSerial :: Word32
                  }

-- | The type of the frontend computations.
type Frontend = WS Inner

-- | Runs a 'Frontend' calculation.
runFrontend :: Frontend a
            -> ObjectManager Server Inner
            -> FrontendState
            -> F FrontendF ((Either ObjectError a, ObjectManager Server Inner), FrontendState)
runFrontend f = runStateT . unInner . runW f

nextEventSerial :: Frontend Word32
nextEventSerial = lift . state $ \s -> (fsEventSerial s + 1, s { fsEventSerial = fsEventSerial s + 1 })

curEventSerial :: Frontend Word32
curEventSerial = lift $ gets fsEventSerial

initialFrontendState :: FrontendState
initialFrontendState =
    FrontendState { fsGlobals     = initialGlobalsData
                  , fsSurfaces    = initialSurfacesData
                  , fsBuffers     = initialBuffersData
                  , fsRegions     = initialRegionsData
                  , fsOutputs     = initialOutputsData
                  , fsWindows     = initialWindowsData
                  , fsEventSerial = 0
                  }
