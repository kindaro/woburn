module Woburn.Frontend.Types.Region
    ( RegionsData
    , initialRegionsData
    )
where

import Data.Int
import Data.Region
import qualified Data.Map as M
import Graphics.Wayland
import Woburn.Protocol.Core

type RegionsData = M.Map (SObject WlRegion) (Region Int32)

initialRegionsData :: RegionsData
initialRegionsData = M.empty
