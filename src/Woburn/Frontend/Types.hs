{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
module Woburn.Frontend.Types
    ( Frontend
    , runFrontend
    , FrontendF (..)
    , sendRequest
    )
where

import Control.Monad.Free.Church
import Control.Monad.Trans
import Graphics.Wayland
import qualified Woburn.Core as C

data FrontendF a =
    SendMessage Message a
  | SendRequest C.Request a
  deriving (Functor)

instance MonadSend (F FrontendF) where
    sendMessage msg = liftF (SendMessage msg ())

sendRequest :: C.Request -> Frontend ()
sendRequest req = lift . liftF $ SendRequest req ()

-- | The type of the frontend computations.
type Frontend = WS (F FrontendF)

-- | Runs a 'Frontend' calculation.
runFrontend :: Frontend a
            -> ObjectManager Server (F FrontendF)
            -> F FrontendF (Either ObjectError a, ObjectManager Server (F FrontendF))
runFrontend = runW
