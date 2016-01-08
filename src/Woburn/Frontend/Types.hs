{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Woburn.Frontend.Types
    ( Frontend
    , FrontendState (..)
    , runFrontend
    , FrontendF (..)
    , sendRequest
    , GlobalCons (..)
    , initialFrontendState
    )
where

import Control.Monad.Free.Church
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Word
import Graphics.Wayland
import qualified Woburn.Core as C
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
    forall i . (DispatchInterface i, Dispatchable Server i) => GlobalCons (SObject i -> Frontend (Slots Server i Frontend))

data FrontendState =
    FrontendState { registries :: S.Set (SObject WlRegistry)
                  , globals    :: M.Map Word32 GlobalCons
                  }

-- | The type of the frontend computations.
type Frontend = WS Inner

-- | Runs a 'Frontend' calculation.
runFrontend :: Frontend a
            -> ObjectManager Server Inner
            -> FrontendState
            -> F FrontendF ((Either ObjectError a, ObjectManager Server Inner), FrontendState)
runFrontend f = runStateT . runW f

initialFrontendState :: FrontendState
initialFrontendState =
    FrontendState { registries = S.empty
                  , globals    = M.empty
                  }
