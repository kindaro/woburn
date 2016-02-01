{-# LANGUAGE FlexibleContexts #-}
module Woburn.Frontend.Output
    ( addOutput
    , removeOutput
    )
where

import Control.Arrow
import Control.Monad.State
import qualified Data.Map as M
import Data.Rect
import Graphics.Wayland
import Linear
import Woburn.Output
import Woburn.Protocol
import Woburn.Frontend.Registry
import Woburn.Frontend.Types

-- | Sends configure and mode events to the client.
configureOutput :: MappedOutput -> SObject WlOutput -> Frontend ()
configureOutput (MappedOutput out (Rect (V2 x y) _)) obj = do
    wlOutputGeometry
        (signals obj) x y
        (outputPhysWidth out)
        (outputPhysHeight out)
        (outputSubpixel out)
        (outputMake out)
        (outputModel out)
        (outputTransform out)
    sendMode True (outputCurMode out)
    mapM_ (sendMode False) (outputModes out)
    wlOutputDone (signals obj)
    where
        sendMode cur mode =
            wlOutputMode
                (signals obj)
                (flags cur (modePreferred mode))
                (modeWidth mode)
                (modeHeight mode)
                (modeRefresh mode)

        flags cur preferred =
            toBitfield ([ WlOutputModeBitsCurrent   | cur       ] ++
                        [ WlOutputModeBitsPreferred | preferred ])

-- | Returns a constructor that can be used to create new 'WlOutput' objects.
outputCons :: MappedOutput -> SignalConstructor Server WlOutput Frontend
outputCons mappedOut@(MappedOutput out _) obj = do
    lift . modify $ \s -> s { outputs = M.adjust (second (obj :)) (outputId out) (outputs s) }
    configureOutput mappedOut obj
    return WlOutputSlots

-- | Adds an output.
--
-- If an output with the same id has already been added, it will simply send
-- out new events telling the client about the new configuration.
addOutput :: MappedOutput -> Frontend ()
addOutput mappedOut@(MappedOutput out _) = do
    info    <- lift . gets $ M.lookup (outputId out) . outputs
    newInfo <- case info of
                 Nothing -> do
                     gid <- addGlobal (outputCons mappedOut)
                     return (gid, [])

                 Just (gid, objs) -> do
                     replaceGlobal gid (outputCons mappedOut)
                     mapM_ (configureOutput mappedOut) objs
                     return (gid, objs)

    lift . modify $ \s -> s { outputs = M.insert (outputId out) newInfo (outputs s) }

-- | Removes an output.
removeOutput :: MappedOutput -> Frontend ()
removeOutput (MappedOutput out _) = do
    info <- lift . gets $ M.lookup (outputId out) . outputs
    lift . modify $ \s -> s { outputs = M.delete (outputId out) (outputs s) }
    case info of
      Nothing       -> error "Trying to remove an output that has not been added"
      Just (gid, _) -> delGlobal gid