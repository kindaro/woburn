module Woburn.Layout
where

import Control.Arrow
import Data.Rect
import Data.Word
import qualified Data.List.Zipper as Z
import Woburn.Output
import Woburn.Universe

layout :: Universe a -> [(MappedOutput, [(Rect Word32, a)])]
layout = map (output &&& layoutScreen) . Z.toList . screens

layoutScreen :: Screen a -> [(Rect Word32, a)]
layoutScreen s = map ((,) (mappedRect $ output s)) . Z.toList . windows $ workspace s
