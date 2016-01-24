{-# LANGUAGE FlexibleContexts #-}
module Woburn.Frontend.Surface
    ( surfaceSlots
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
            unregisterObject surface

        attach mBufObj x y = do
            buf <- case mBufObj of
                     Nothing     -> return Nothing
                     Just bufObj -> Just <$> acquireBuffer bufObj
            modifySurface $ \s -> s { fsBuffer       = buf
                                    , fsBufferOffset = V2 x y + fsBufferOffset s
                                    }

        damage x y w h = modifySurface $ \s -> s { fsDamageSurface = R.add (mkRect x y w h) (fsDamageSurface s) }
        damageBuffer x y w h = modifySurface $ \s -> s { fsDamageBuffer = R.add (mkRect x y w h) (fsDamageBuffer s) }

        frame callback = undefined

        setOpaque region = do
            reg <- lift $ gets ((region >>=) . flip M.lookup . regions)
            modifySurface $ \s -> s { fsOpaque = fromMaybe R.everything reg }

        setInput region = do
            reg <- lift $ gets ((region >>=) . flip M.lookup . regions)
            modifySurface $ \s -> s { fsInput = fromMaybe R.everything reg }

        commit = do
            s <- lift . gets $ fmap toSurfaceState . M.lookup surface . surfaceData
            case s of
              Nothing -> error "Surface without surface data, should not happen"
              Just st -> sendRequest $ C.SurfaceCommit surfaceId st
            modifySurface nextSurfaceData

        setBufferTransform t' =
            case fromInt32 t' of
              Nothing -> displayError surface WlSurfaceErrorInvalidTransform "Invalid buffer transform"
              Just t  -> modifySurface $ \s -> s { fsBufferTransform = t }

        setBufferScale scale =
            if scale > 0
              then modifySurface $ \s -> s { fsBufferScale = scale }
              else displayError surface WlSurfaceErrorInvalidScale "Invalid buffer scale"
