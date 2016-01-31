{-# LANGUAGE FlexibleContexts #-}
module Woburn.Frontend.Display.Object
    ( display
    , displayError
    , destroyClientObject
    )
where

import Graphics.Wayland
import Woburn.Frontend.Types
import Woburn.Protocol

-- | The global display object.
display :: SObject WlDisplay
display = Object 1

-- | Signals the client that an error has occured.
displayError :: WireEnum e => Object Server i -> e -> String -> Frontend ()
displayError (Object objId) = wlDisplayError (signals display) objId

-- | Unregisters a client object, and signals the client that it has received
-- the destroy request.
destroyClientObject :: Dispatchable Server i => Object Server i -> Frontend ()
destroyClientObject obj@(Object objId) = do
    wlDisplayDeleteId (signals display) (unObjId objId)
    unregisterObject obj
