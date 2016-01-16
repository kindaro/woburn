module Woburn.Frontend.Display.Object
    ( display
    , displayError
    )
where

import Graphics.Wayland
import Woburn.Frontend.Types
import Woburn.Protocol

-- | The global display object.
display :: SObject WlDisplay
display = Object 1

displayError :: WireEnum e => Object Server i -> e -> String -> Frontend ()
displayError (Object objId) = wlDisplayError (signals display) objId
