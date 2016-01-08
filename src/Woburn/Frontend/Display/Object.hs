module Woburn.Frontend.Display.Object
    ( display
    )
where

import Graphics.Wayland
import Woburn.Protocol

-- | The global display object.
display :: SObject WlDisplay
display = Object 1

