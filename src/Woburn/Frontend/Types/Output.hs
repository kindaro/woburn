module Woburn.Frontend.Types.Output
    ( OutputsData
    , initialOutputsData
    )
where

import qualified Data.Map as M
import Graphics.Wayland
import Woburn.Frontend.Types.Global
import Woburn.Output
import Woburn.Protocol

type OutputsData = M.Map OutputId (GlobalId, [SObject WlOutput])

initialOutputsData :: OutputsData
initialOutputsData = M.empty
