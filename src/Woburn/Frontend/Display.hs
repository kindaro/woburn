{-# LANGUAGE FlexibleContexts #-}
module Woburn.Frontend.Display
    ( display
    , displaySlots
    )
where

import Control.Monad.State
import Data.Int
import qualified Data.Set as S
import Graphics.Wayland
import Woburn.Frontend.Display.Object
import Woburn.Frontend.Registry
import Woburn.Frontend.Types
import Woburn.Protocol

displaySlots :: Slots Server WlDisplay Frontend
displaySlots =
    WlDisplaySlots { wlDisplaySync        = displaySync
                   , wlDisplayGetRegistry = displayGetRegistry
                   }
    where
        -- TODO: "callback_data is the event serial" -- what is the event serial?
        displaySync callbackCons = do
            callback <- callbackCons (\_ -> return WlCallbackSlots)
            wlCallbackDone (signals callback) (0 :: Int32)
            unregisterObject callback

        displayGetRegistry registryCons = do
            reg <- registryCons $ return . const registrySlots
            lift $ modify (\s -> s { registries = S.insert reg (registries s) })
