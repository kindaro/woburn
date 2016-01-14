{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Woburn.Frontend.Registry
    ( registrySlots
    , announceGlobals
    , addGlobal
    , delGlobal
    )
where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set.Diet as D
import Data.Word
import Graphics.Wayland
import Woburn.Frontend.Display.Object
import Woburn.Frontend.Types
import Woburn.Protocol

-- | Announces a single global to a registry object.
announceGlobal :: SObject WlRegistry -> GlobalId -> GlobalCons -> Frontend ()
announceGlobal reg globalId (GlobalCons cons) =
    wlRegistryGlobal (signals reg) globalId (consName cons) (consVer cons)

-- | Announces all the currently added globals to a registry object.
announceGlobals :: SObject WlRegistry -> Frontend ()
announceGlobals reg =
    lift (gets globals) >>= mapM_ (uncurry (announceGlobal reg)) . M.toList

-- | Adds a new global object and sends an announcement to all registry objects.
addGlobal :: (DispatchInterface i, Dispatchable Server i) => SignalConstructor Server i Frontend -> Frontend GlobalId
addGlobal cons = do
    let globalCons = GlobalCons cons

    gid <- lift . state $ \s ->
        case D.minView (globalIds s) of
          Nothing      -> error "out of global IDs"
          Just (a, gs) -> (a, s { globalIds = gs, globals = M.insert a globalCons (globals s) })

    mapM_ (\r -> announceGlobal r gid globalCons) =<< lift (gets registries)
    return gid

-- | Deletes a global object and sends an announcement to all registry objects.
delGlobal :: GlobalId -> Frontend ()
delGlobal gid = do
    lift . modify $ \s ->
        s { globalIds = D.insert gid (globalIds s)
          , globals   = M.delete gid (globals s)
          }
    mapM_ (\r -> wlRegistryGlobalRemove (signals r) gid) =<< lift (gets registries)

-- | Returns 'Slots' for the 'WlRegistry' object.
registrySlots :: Slots Server WlRegistry Frontend
registrySlots = WlRegistrySlots { wlRegistryBind = registryBind }
    where
        registryBind :: Word32
                     -> (forall i. (DispatchInterface i, Dispatchable Server i) => SlotConstructor Server i Frontend)
                     -> Frontend ()
        registryBind globalId cons = do
            gs <- M.lookup (GlobalId globalId) <$> lift (gets globals)
            case gs of
              Just (GlobalCons slots) -> void (cons slots)
              Nothing                 ->
                  wlDisplayError (signals display) (fromIntegral globalId) WlDisplayErrorInvalidObject "unknown object"
