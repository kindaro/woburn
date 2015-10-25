module Woburn.Backend
where

import Data.Rect
import Data.STree
import Data.Int
import Data.Word
import Linear
import Woburn.Output
import Woburn.Surface

data Request s =
    OutputSetMode OutputId Word
  | SurfaceCommit [Surface s] [(OutputId, [(Rect Word32, STree (V2 Int32, s))])]
  deriving (Eq, Show)

data Event =
    OutputAdded Output
  | OutputRemoved OutputId
  deriving (Eq, Show)
