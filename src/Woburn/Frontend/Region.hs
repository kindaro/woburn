{-# LANGUAGE FlexibleContexts #-}

module Woburn.Frontend.Region
    ( regionSlots
    , mkRect
    )
where

import Control.Arrow
import Control.Monad.State
import Data.Int
import qualified Data.Map as M
import qualified Data.Region as R
import Linear
import Graphics.Wayland
import Woburn.Frontend.Display.Object
import Woburn.Frontend.Types
import Woburn.Protocol.Core

mkRect :: Int32 -> Int32 -> Int32 -> Int32 -> R.Rect Int32
mkRect x y w h = R.Rect (V2 x y) (V2 (x + w - 1) (y + h - 1))

regionSlots :: SignalConstructor Server WlRegion Frontend
regionSlots reg = do
    modify . second $ \s -> s { fsRegions = M.insert reg R.empty (fsRegions s) }
    return
        WlRegionSlots { wlRegionDestroy  = regionDestroy
                      , wlRegionAdd      = regionAdd
                      , wlRegionSubtract = regionSub
                      }
    where
        regionDestroy = do
            modify . second $ \s -> s { fsRegions = M.delete reg (fsRegions s) }
            destroyClientObject reg

        regionAdd x y w h =
            modify . second $ \s -> s { fsRegions = M.adjust (R.add $ mkRect x y w h) reg (fsRegions s) }

        regionSub x y w h =
            modify . second $ \s -> s { fsRegions = M.adjust (R.sub $ mkRect x y w h) reg (fsRegions s) }
