{-# LANGUAGE FlexibleContexts #-}

module Woburn.Frontend.Region
    ( regionSlots
    , mkRect
    )
where

import Control.Monad.State
import Data.Int
import qualified Data.Map as M
import qualified Data.Region as R
import Linear
import Graphics.Wayland
import Woburn.Frontend.Types
import Woburn.Protocol

mkRect :: Int32 -> Int32 -> Int32 -> Int32 -> R.Rect Int32
mkRect x y w h = R.Rect (V2 x y) (V2 (x + w - 1) (y + h - 1))

regionSlots :: SignalConstructor Server WlRegion Frontend
regionSlots reg = do
    lift . modify $ \s -> s { regions = M.insert reg R.empty (regions s) }
    return
        WlRegionSlots { wlRegionDestroy  = regionDestroy
                      , wlRegionAdd      = regionAdd
                      , wlRegionSubtract = regionSub
                      }
    where
        regionDestroy = do
            lift . modify $ \s -> s { regions = M.delete reg (regions s) }
            unregisterObject reg

        regionAdd x y w h =
            lift . modify $ \s -> s { regions = M.adjust (R.add $ mkRect x y w h) reg (regions s) }

        regionSub x y w h =
            lift . modify $ \s -> s { regions = M.adjust (R.sub $ mkRect x y w h) reg (regions s) }
