{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Woburn.Frontend.Registry
    ( registrySlots
    , announceGlobals
    , addGlobal
    , delGlobal
    , replaceGlobal
    )
where

import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import Data.Word
import Graphics.Wayland
import Woburn.Frontend.Types
import Woburn.Frontend.Types.Global as G
import Woburn.Protocol.Core

-- | Announces a single global to a registry object.
announceGlobal :: SObject WlRegistry -> GlobalId -> GlobalCons Frontend -> Frontend ()
announceGlobal reg globalId (GlobalCons cons) =
    wlRegistryGlobal (signals reg) globalId (consName cons) (consVer cons)

-- | Announces all the currently added globals to a registry object.
announceGlobals :: SObject WlRegistry -> Frontend ()
announceGlobals reg =
    gets (M.toList . globals . fsGlobals . snd) >>= mapM_ (uncurry (announceGlobal reg))

-- | Adds a new global object and sends an announcement to all registry objects.
addGlobal :: (DispatchInterface i, Dispatchable Server i) => SignalConstructor Server i Frontend -> Frontend GlobalId
addGlobal cons = do
    res <- G.insert globalCons <$> gets (fsGlobals . snd)
    case res of
      Nothing        -> throwError $ ErrUser "out of global IDs"
      Just (gid, gs) -> do
          modify . second $ \s -> s { fsGlobals = gs }
          mapM_ (\r -> announceGlobal r gid globalCons) =<< gets (registries . fsGlobals . snd)
          return gid
    where
        globalCons = GlobalCons cons

-- | Deletes a global object and sends an announcement to all registry objects.
delGlobal :: GlobalId -> Frontend ()
delGlobal gid = do
    modify . second $ \s -> s { fsGlobals = G.delete gid (fsGlobals s) }
    mapM_ (\r -> wlRegistryGlobalRemove (signals r) gid) =<< gets (registries . fsGlobals . snd)

-- | Replaces the constructor for a global object.
replaceGlobal :: (DispatchInterface i, Dispatchable Server i)
              => GlobalId
              -> SignalConstructor Server i Frontend
              -> Frontend ()
replaceGlobal gid cons =
    modify . second $ \s -> s { fsGlobals = G.replace gid (GlobalCons cons) (fsGlobals s) }

-- | Returns 'Slots' for the 'WlRegistry' object.
registrySlots :: Slots Server WlRegistry Frontend
registrySlots = WlRegistrySlots { wlRegistryBind = registryBind }
    where
        registryBind :: Word32
                     -> (forall i. (DispatchInterface i, Dispatchable Server i) => SlotConstructor Server i Frontend)
                     -> Frontend ()
        registryBind globalId cons = do
            gs <- G.lookup (GlobalId globalId) <$> gets (fsGlobals . snd)
            case gs of
              Just (GlobalCons slots) -> void (cons slots)
              Nothing                 -> protocolError (fromIntegral globalId) "Unknown global object"
