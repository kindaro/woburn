{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Woburn.Frontend.Types
    ( Frontend
    , FrontendState (..)
    , FrontendSurfaceData (..)
    , runFrontend
    , FrontendF (..)
    , sendRequest
    , GlobalCons (..)
    , GlobalId (..)
    , initialFrontendState
    , nextEventSerial
    , curEventSerial
    )
where

import Control.Monad.Free.Church
import Control.Monad.State
import Data.Int
import Data.Region
import Data.Word
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Set.Diet as D
import Graphics.Wayland
import Linear
import qualified Woburn.Core as C
import Woburn.Buffer
import Woburn.Protocol

data FrontendF a =
    SendMessage Message a
  | SendRequest C.Request a
  deriving (Functor)

type Inner = StateT FrontendState (F FrontendF)

instance MonadSend Inner where
    sendMessage msg = lift . liftF $ SendMessage msg ()

sendRequest :: C.Request -> Frontend ()
sendRequest req = lift . liftF $ SendRequest req ()

data GlobalCons =
    forall i . (DispatchInterface i, Dispatchable Server i) => GlobalCons (SignalConstructor Server i Frontend)

newtype GlobalId = GlobalId Word32
    deriving (Eq, Show, Ord, Enum, Bounded, WireEnum)

data FrontendSurfaceData =
    FrontendSurfaceData { fsDamageSurface   :: Region Int32
                        , fsDamageBuffer    :: Region Int32
                        , fsOpaque          :: Region Int32
                        , fsInput           :: Region Int32
                        , fsBuffer          :: Maybe Buffer
                        , fsBufferOffset    :: V2 Int32
                        , fsBufferTransform :: WlOutputTransform
                        , fsBufferScale     :: Int32
                        }

data FrontendState =
    FrontendState { registries  :: S.Set (SObject WlRegistry)
                  , globals     :: M.Map GlobalId GlobalCons
                  , globalIds   :: D.Diet GlobalId
                  , regions     :: M.Map (SObject WlRegion) (Region Int32)
                  , eventSerial :: Word32
                  , surfaceData :: M.Map (SObject WlSurface) FrontendSurfaceData
                  , buffers     :: M.Map (SObject WlBuffer) Buffer
                  }

-- | The type of the frontend computations.
type Frontend = WS Inner

-- | Runs a 'Frontend' calculation.
runFrontend :: Frontend a
            -> ObjectManager Server Inner
            -> FrontendState
            -> F FrontendF ((Either ObjectError a, ObjectManager Server Inner), FrontendState)
runFrontend f = runStateT . runW f

nextEventSerial :: Frontend Word32
nextEventSerial = lift . state $ \s -> (eventSerial s + 1, s { eventSerial = eventSerial s + 1 })

curEventSerial :: Frontend Word32
curEventSerial = lift $ gets eventSerial

initialFrontendState :: FrontendState
initialFrontendState =
    FrontendState { registries  = S.empty
                  , globals     = M.empty
                  , globalIds   = D.singletonI $ D.Interval minBound maxBound
                  , regions     = M.empty
                  , eventSerial = 0
                  , surfaceData = M.empty
                  , buffers     = M.empty
                  }
