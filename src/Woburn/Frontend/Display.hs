{-# LANGUAGE FlexibleContexts #-}
module Woburn.Frontend.Display
    ( display
    , displaySlots
    )
where

import Control.Monad.State
import Graphics.Wayland
import Woburn.Frontend.Callback
import Woburn.Frontend.Display.Object
import Woburn.Frontend.Registry
import Woburn.Frontend.Types
import Woburn.Frontend.Types.Global
import Woburn.Protocol.Core

displaySlots :: Slots Server WlDisplay Frontend
displaySlots =
    WlDisplaySlots { wlDisplaySync        = displaySync
                   , wlDisplayGetRegistry = displayGetRegistry
                   }
    where
        displaySync callbackCons = do
            callback <- callbackCons (\_ -> return WlCallbackSlots)
            serial   <- curEventSerial
            callbackDone serial callback

        displayGetRegistry registryCons = do
            reg <- registryCons $ return . const registrySlots
            lift $ modify (\s -> s { fsGlobals = insertRegistry reg (fsGlobals s) })
            announceGlobals reg
