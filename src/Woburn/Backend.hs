module Woburn.Backend
where

import Data.Int
import Data.Rect
import Data.Word
import Linear
import Woburn.Buffer
import Woburn.Output
import Woburn.Surface

-- | Describes a request from the core to the backend.
data Request s =
    -- | Changes an outputs mode.
    OutputSetMode OutputId Word
    -- | Commits a set of surfaces, and returns the current layout.
    --
    -- The layout is a list of outpus, with their windows.
    --
    -- The windows have a rectangular outline, and a list of surfaces. The
    -- surfaces are ordered with the top-most surface first.. The surfaces are
    -- paired with their offset from the upper left corner of the output.
  | SurfaceCommit [Surface s ()] [(OutputId, [(Rect Word32, [(V2 Int32, Surface s ())])])]
    -- | Notifies the backend that a set of surfaces has been destroyed.
  | SurfaceDestroy [Surface s ()]
  deriving (Eq, Show)

-- | Describes an event from the backend to the core.
data Event =
    OutputAdded OutputId Output
  | OutputRemoved OutputId
  | OutputFrame OutputId
  | BufferReleased Buffer
  deriving (Eq, Show)
