{-# LANGUAGE FlexibleContexts #-}
module Woburn.Frontend.Surface
    ( surfaceSlots
    , surfaceFrame
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
import Woburn.Frontend.Types
import Woburn.Protocol
import Woburn.Surface

nextSurfaceData :: FrontendSurfaceData -> FrontendSurfaceData
nextSurfaceData fs =
    fs { fsDamageSurface = R.empty
       , fsDamageBuffer  = R.empty
       , fsBuffer        = Nothing
       }

toSurfaceState :: FrontendSurfaceData -> SurfaceState
toSurfaceState fs =
    SurfaceState { surfBuffer       = fsBuffer fs
                 , surfBufferOffset = fsBufferOffset fs
                 , surfBufferScale  = fsBufferScale fs
                 , surfDamage       = combinedDamage
                 , surfOpaque       = fsOpaque fs
                 , surfInput        = fsInput fs
                 , surfTransform    = fsBufferTransform fs
                 }
    where
        combinedDamage =
            fsDamageBuffer fs
            `R.union`
            R.scale (fsBufferScale fs) (R.offset (- fsBufferOffset fs) (fsDamageSurface fs))

initialSurfaceData :: FrontendSurfaceData
initialSurfaceData =
    FrontendSurfaceData { fsDamageSurface   = R.empty
                        , fsDamageBuffer    = R.empty
                        , fsOpaque          = R.empty
                        , fsInput           = R.everything
                        , fsBuffer          = Nothing
                        , fsBufferOffset    = 0
                        , fsBufferTransform = WlOutputTransformNormal
                        , fsBufferScale     = 1
                        , fsFrameCallbacks  = []
                        }

surfaceSlots :: SignalConstructor Server WlSurface Frontend
surfaceSlots surface = do
    sendRequest $ C.SurfaceCreate surfaceId
    lift . modify $ \s -> s { surfaceData = M.insert surface initialSurfaceData (surfaceData s) }
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
        surfaceId = fromIntegral . unObjId . unObject $ surface

        modifySurface f = lift . modify $ \s -> s { surfaceData = M.adjust f surface (surfaceData s) }

        destroy = do
            sendRequest $ C.SurfaceDestroy surfaceId
            lift . modify $ \s -> s { surfaceData = M.delete surface (surfaceData s) }
            destroyClientObject surface

        attach mBufObj x y = do
            buf <- case mBufObj of
                     Nothing     -> return Nothing
                     Just bufObj -> Just <$> acquireBuffer bufObj
            modifySurface $ \s -> s { fsBuffer       = buf
                                    , fsBufferOffset = V2 x y + fsBufferOffset s
                                    }

        damage x y w h = modifySurface $ \s -> s { fsDamageSurface = R.add (mkRect x y w h) (fsDamageSurface s) }
        damageBuffer x y w h = modifySurface $ \s -> s { fsDamageBuffer = R.add (mkRect x y w h) (fsDamageBuffer s) }

        frame callbackCons = do
            callback <- callbackCons (\_ -> return WlCallbackSlots)
            modifySurface $ \s -> s { fsFrameCallbacks = callback : fsFrameCallbacks s }

        setOpaque region = do
            reg <- lift $ gets ((region >>=) . flip M.lookup . regions)
            modifySurface $ \s -> s { fsOpaque = fromMaybe R.everything reg }

        setInput region = do
            reg <- lift $ gets ((region >>=) . flip M.lookup . regions)
            modifySurface $ \s -> s { fsInput = fromMaybe R.everything reg }

        commit = do
            sData     <- lift . gets $ M.lookup surface . surfaceData
            callbacks <- case sData of
                           Nothing   -> error "Surface without surface data, should not happen"
                           Just surf -> do
                               sendRequest . C.SurfaceCommit surfaceId $ toSurfaceState surf
                               return $ fsFrameCallbacks surf

            lift . modify $ \s -> s { frameCallbacks = M.insertWith (++) surfaceId callbacks (frameCallbacks s) }
            modifySurface nextSurfaceData

        setBufferTransform t' =
            case fromInt32 t' of
              Nothing -> displayError surface WlSurfaceErrorInvalidTransform "Invalid buffer transform"
              Just t  -> modifySurface $ \s -> s { fsBufferTransform = t }

        setBufferScale scale =
            if scale > 0
              then modifySurface $ \s -> s { fsBufferScale = scale }
              else displayError surface WlSurfaceErrorInvalidScale "Invalid buffer scale"

-- | Sends frame callbacks to all the given surfaces.
surfaceFrame :: [SurfaceId] -> Frontend ()
surfaceFrame sids = do
    ts <- getTimestamp
    mapM_ (f ts) sids
    where
        f ts sid = do
            callbacks <-
                fmap (fromMaybe [])
                . lift
                . state
                $ \s -> ( M.lookup sid (frameCallbacks s)
                        , s { frameCallbacks = M.delete sid (frameCallbacks s) }
                        )
            mapM_ (callbackDone ts) (reverse callbacks)
