module Woburn.Frontend.Callback
    ( callbackDone
    )
where

import Graphics.Wayland
import Woburn.Frontend.Display.Object
import Woburn.Frontend.Types
import Woburn.Protocol

-- | Calls and deletes a 'WlCallback' object.
callbackDone :: WireEnum e => e -> SObject WlCallback -> Frontend ()
callbackDone a obj = do
    wlCallbackDone (signals obj) a
    destroyClientObject obj
