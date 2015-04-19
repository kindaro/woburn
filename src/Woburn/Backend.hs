module Woburn.Backend
    ( BackendFunctions (..)
    , BackendCallbacks (..)
    , BackendConstructor
    )
where

import Woburn.Output
import Woburn.Surface

-- | Type of backend constructor functions.
type BackendConstructor s o = BackendCallbacks s o -> IO (BackendFunctions s o)

-- | A set of functions used by the composer to talk to the backend.
data BackendFunctions s o =
    BackendFunctions { -- | Commits the surface's current 'Buffer' to the backend data.
                       backSurfCommit :: Surface s -> IO (),
                       -- | Creates the backend specific data for a new surface.
                       backSurfCreate :: IO s,
                       -- | Composes the 'SurfaceSet' to the specified 'Output'.
                       backOutCompose :: Output o -> SurfaceSet s -> IO (),
                       -- | Selects the mode of an 'Output'.
                       backOutSetMode :: Output o -> Int -> IO ()
                     }

-- | A set of functions the backend can use to send back notifications to the composer.
data BackendCallbacks s o =
    BackendCallbacks { -- | Adds a new 'Output', or updates an existing one.
                       backOutAdd     :: Output o -> IO (),
                       -- | Removes an 'Output'
                       backOutRemove  :: Output o -> IO (),
                       -- | Called when a new frame has been dispatched.
                       backOutFrame   :: Output o -> IO ()
                     }
