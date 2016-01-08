{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Woburn.Frontend.Registry
where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import Data.Word
import Graphics.Wayland
import Woburn.Frontend.Display.Object
import Woburn.Frontend.Types
import Woburn.Protocol

registrySlots :: Slots Server WlRegistry Frontend
registrySlots = WlRegistrySlots { wlRegistryBind = registryBind }
    where
        registryBind :: Word32
                     -> (forall i. (DispatchInterface i, Dispatchable Server i) => SlotConstructor Server i Frontend)
                     -> Frontend ()
        registryBind globalId cons = do
            gs <- M.lookup globalId <$> lift (gets globals) 
            case gs of
              Just (GlobalCons slots) -> void (cons slots)
              Nothing                 ->
                  wlDisplayError (signals display) (fromIntegral globalId) WlDisplayErrorInvalidObject "unknown object"
