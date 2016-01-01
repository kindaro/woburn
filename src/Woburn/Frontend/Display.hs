{-# LANGUAGE FlexibleContexts #-}
module Woburn.Frontend.Display
where

import Data.Int
import Graphics.Wayland
import Woburn.Protocol

-- | The global display object.
display :: Object Server WlDisplay
display = Object 1


displaySlots :: MonadDispatch Server m => Slots Server WlDisplay m
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

        displayGetRegistry registryCons = undefined
