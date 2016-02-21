{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Woburn.Frontend.Types.Global
    ( GlobalCons (..)
    , GlobalId (..)
    , GlobalsData (..)
    , initialGlobalsData
    , insert
    , delete
    , replace
    , lookup
    , insertRegistry
    )
where

import Data.Word
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Set.Diet as D
import Graphics.Wayland
import Prelude hiding (lookup)
import Woburn.Protocol

data GlobalCons m =
    forall i . (DispatchInterface i, Dispatchable Server i) => GlobalCons (SignalConstructor Server i m)

newtype GlobalId = GlobalId Word32
    deriving (Eq, Show, Ord, Enum, Bounded, WireEnum)

data GlobalsData m =
    GlobalsData { registries :: S.Set (SObject WlRegistry)
                , globals    :: M.Map GlobalId (GlobalCons m)
                , globalIds  :: D.Diet GlobalId
                }

-- | Inserts a global and returns its 'GlobalId', or 'Nothing' if there are no
-- more available IDs.
insert :: GlobalCons m -> GlobalsData m -> Maybe (GlobalId, GlobalsData m)
insert cons gd = do
    (gid, gs) <- D.minView (globalIds gd)
    return (gid, gd { globalIds = gs, globals = M.insert gid cons (globals gd) })

insertRegistry :: SObject WlRegistry -> GlobalsData m -> GlobalsData m
insertRegistry reg bd = bd { registries = S.insert reg (registries bd) }

-- | Deletes a global from 'GlobalsData'.
delete :: GlobalId -> GlobalsData m -> GlobalsData m
delete gid gd =
    gd { globalIds = D.insert gid (globalIds gd)
       , globals   = M.delete gid (globals   gd)
       }

replace :: GlobalId -> GlobalCons m -> GlobalsData m -> GlobalsData m
replace gid cons gd =
    gd { globals = M.insert gid cons (globals gd) }

lookup :: GlobalId -> GlobalsData m -> Maybe (GlobalCons m)
lookup gid = M.lookup gid . globals

-- | The initial state for global objects.
initialGlobalsData :: GlobalsData m
initialGlobalsData =
    GlobalsData { registries = S.empty
                , globals    = M.empty
                , globalIds  = D.singletonI $ D.Interval minBound maxBound
                }
