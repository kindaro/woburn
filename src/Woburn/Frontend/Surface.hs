{-# LANGUAGE FlexibleContexts #-}
module Woburn.Frontend.Surface
    ( surfaceSlots
    , surfaceFrame
    , surfaceToId
    )
where

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

toSurfaceState :: SurfaceData -> SurfaceState
toSurfaceState sd =
    SurfaceState { surfBuffer       = sdBuffer sd
                 , surfBufferOffset = sdBufferOffset sd
                 , surfBufferScale  = sdBufferScale sd
                 , surfDamage       = combinedDamage
                 , surfOpaque       = sdOpaque sd
                 , surfInput        = sdInput sd
                 , surfTransform    = sdBufferTransform sd
                 }
    where
        combinedDamage =
            sdDamageBuffer sd
            `R.union`
            R.scale (sdBufferScale sd) (R.offset (- sdBufferOffset sd) (sdDamageSurface sd))

surfaceSlots :: SignalConstructor Server WlSurface Frontend
surfaceSlots surface = do
    sendRequest $ C.SurfaceCreate surfaceId
    lift . modify $ \s -> s { fsSurfaces = insertSurface surface initialSurfaceData (fsSurfaces s) }
    return
        WlSurfaceSlots { wlSurfaceDestroy            = destroy
                       , wlSurfaceAttach             = attach
                       , wlSurfaceDamage             = damage
                       , wlSurfaceFrame              = frame
                       , wlSurfaceSetOpaqueRegion    = setOpaque
                       , wlSurfaceSetInputRegion     = setInput
                       , wlSurfaceCommit             = commit
                       , wlSurfaceSetBufferTransform = setBufferTransform
                       , wlSurfaceSetBufferScale     = setBufferScale
                       , wlSurfaceDamageBuffer       = damageBuffer
                       }
    where
        surfaceId = surfaceToId surface

        modifySurface f = lift . modify $ \s -> s { fsSurfaces = adjustSurface f surface (fsSurfaces s) }

        destroy = do
            sendRequest $ C.SurfaceDestroy surfaceId
            subsurface <- lift $ gets ((>>= sdSubsurface) . lookupSurface surface . fsSurfaces)
            lift . modify $ \s -> s { fsSurfaces = deleteSurface surface (fsSurfaces s) }
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
            reg <- lift $ gets ((region >>=) . flip M.lookup . fsRegions)
            modifySurface $ \s -> s { sdOpaque = fromMaybe R.everything reg }

        setInput region = do
            reg <- lift $ gets ((region >>=) . flip M.lookup . fsRegions)
            modifySurface $ \s -> s { sdInput = fromMaybe R.everything reg }

        commit = do
            sData <- lift . gets $ lookupSurface surface . fsSurfaces
            cbs   <- case sData of
                       Nothing   -> error "Surface without surface data, should not happen"
                       Just surf -> do
                           sendRequest . C.SurfaceCommit surfaceId $ toSurfaceState surf
                           return $ sdFrameCallbacks surf

            lift . modify $ \s -> s { fsSurfaces = insertCallbacks surfaceId cbs (fsSurfaces s) }
            modifySurface nextSurfaceData

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
            cbs <- lift . state $ \s ->
                ( lookupCallbacks sid (fsSurfaces s)
                , s { fsSurfaces = deleteCallbacks sid (fsSurfaces s) }
                )
            mapM_ (callbackDone ts) (reverse cbs)
