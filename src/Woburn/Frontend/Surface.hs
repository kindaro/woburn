{-# LANGUAGE FlexibleContexts #-}
module Woburn.Frontend.Surface
    ( surfaceSlots
    , surfaceFrame
    , surfaceToId
    )
where

import Control.Arrow
import Control.Monad.State
import Data.Maybe
import qualified Data.Region as R
import qualified Data.Map as M
import Graphics.Wayland
import Linear
import qualified Woburn.Core as C
import Woburn.Frontend.Buffer
import Woburn.Frontend.Callback
import Woburn.Frontend.Display.Object
import Woburn.Frontend.Region
import Woburn.Frontend.Subsurface
import Woburn.Frontend.Types
import Woburn.Frontend.Types.Surface
import Woburn.Protocol.Core
import Woburn.Surface

surfaceSlots :: SignalConstructor Server WlSurface Frontend
surfaceSlots surface = do
    sendRequest $ C.SurfaceCreate surfaceId
    modify . second $ \s -> s { fsSurfaces = insertSurface surface initialSurfaceData (fsSurfaces s) }
    return
        WlSurfaceSlots { wlSurfaceDestroy            = destroy
                       , wlSurfaceAttach             = attach
                       , wlSurfaceDamage             = damage
                       , wlSurfaceFrame              = frame
                       , wlSurfaceSetOpaqueRegion    = setOpaque
                       , wlSurfaceSetInputRegion     = setInput
                       , wlSurfaceCommit             = commit'
                       , wlSurfaceSetBufferTransform = setBufferTransform
                       , wlSurfaceSetBufferScale     = setBufferScale
                       , wlSurfaceDamageBuffer       = damageBuffer
                       }
    where
        surfaceId = surfaceToId surface

        modifySurfaces f = modify . second $ \s -> s { fsSurfaces = f (fsSurfaces s) }
        modifySurface f = modifySurfaces (adjustSurface f surface)

        destroy = do
            sendRequest $ C.SurfaceDestroy surfaceId
            subsurface <- gets $ (>>= sdSubsurface) . lookupSurface surface . fsSurfaces . snd
            modify . second $ \s -> s { fsSurfaces = deleteSurface surface (fsSurfaces s) }
            destroyClientObject surface
            maybe (return ()) makeSubsurfaceInert subsurface

        attach mBufObj x y = do
            buf <- case mBufObj of
                     Nothing     -> return Nothing
                     Just bufObj -> Just <$> acquireBuffer bufObj
            modifySurface $ \s -> s { sdBuffer       = buf
                                    , sdBufferOffset = V2 x y + sdBufferOffset s
                                    }

        damage x y w h = modifySurface $ \s -> s { sdDamageSurface = R.add (mkRect x y w h) (sdDamageSurface s) }
        damageBuffer x y w h = modifySurface $ \s -> s { sdDamageBuffer = R.add (mkRect x y w h) (sdDamageBuffer s) }

        frame callbackCons = do
            callback <- callbackCons (\_ -> return WlCallbackSlots)
            modifySurface $ \s -> s { sdFrameCallbacks = callback : sdFrameCallbacks s }

        setOpaque region = do
            reg <- gets $ (region >>=) . flip M.lookup . fsRegions . snd
            modifySurface $ \s -> s { sdOpaque = fromMaybe R.everything reg }

        setInput region = do
            reg <- gets $ (region >>=) . flip M.lookup . fsRegions . snd
            modifySurface $ \s -> s { sdInput = fromMaybe R.everything reg }

        commit' = do
            surfs <- state $ \(o, s) ->
                second (\sd -> (o, s { fsSurfaces = sd }))
                . commit surface $ fsSurfaces s
            sendRequest $ C.SurfaceCommit surfs

        setBufferTransform t' =
            case fromInt32 t' of
              Nothing -> displayError surface WlSurfaceErrorInvalidTransform "Invalid buffer transform"
              Just t  -> modifySurface $ \s -> s { sdBufferTransform = t }

        setBufferScale scale =
            if scale > 0
              then modifySurface $ \s -> s { sdBufferScale = scale }
              else displayError surface WlSurfaceErrorInvalidScale "Invalid buffer scale"

-- | Sends frame callbacks to all the given surfaces.
surfaceFrame :: [SurfaceId] -> Frontend ()
surfaceFrame sids = do
    ts <- getTimestamp
    mapM_ (f ts) sids
    where
        f ts sid = do
            cbs <- state $ \(o, s) ->
                ( lookupCallbacks sid (fsSurfaces s)
                , (o, s { fsSurfaces = deleteCallbacks sid (fsSurfaces s) })
                )
            mapM_ (callbackDone ts) (reverse cbs)
