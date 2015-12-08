module Woburn.Backend
where

import Data.Int
import Data.Rect
import Data.STree
import Data.Word
import Linear
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
    -- The windows have a rectangular outline, and a tree of surfaces. The
    -- surfaces are paired with their offset from the upper left corner of the
    -- output.
  | SurfaceCommit [Surface s] [(OutputId, [(Rect Word32, STree (V2 Int32, s))])]
  deriving (Eq, Show)

-- | Describes an event from the backend to the core.
data Event =
    OutputAdded Output
  | OutputRemoved OutputId
  deriving (Eq, Show)
