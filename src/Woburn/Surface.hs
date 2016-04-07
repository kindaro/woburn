{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Woburn.Surface
    ( SurfaceId
    , Surface (..)
    , SurfaceState (..)
    , modifyState
    , create
    )
where

import Data.Int
import Data.Region
import Data.Word
import Linear
import Woburn.Buffer
import Woburn.Protocol.Core

newtype SurfaceId = SurfaceId Word32
    deriving (Eq, Ord, Show, Num, Real, Integral, Enum)

data SurfaceState a =
    SurfaceState { surfBuffer       :: Maybe Buffer
                 , surfBufferOffset :: V2 Int32
                 , surfBufferScale  :: Int32
                 , surfDamage       :: Region Int32
                 , surfOpaque       :: Region Int32
                 , surfInput        :: Region Int32
                 , surfTransform    :: WlOutputTransform
                 , surfChildren     :: [(V2 Int32, a)]
                 }
    deriving (Eq, Show)

data Surface s a =
    Surface { surfState :: SurfaceState a -- ^ The current surface state.
            , surfData  :: s              -- ^ Internal data used by the backend.
            }
    deriving (Eq, Show)

-- | Modifies the surface state.
modifyState :: (SurfaceState a -> SurfaceState b) -> Surface s a -> Surface s b
modifyState f s = s { surfState = f (surfState s) }

-- | Creates a new surface.
create :: s -> Surface s a
create s =
    Surface { surfState = initialState
            , surfData  = s
            }
    where
        initialState =
            SurfaceState { surfBuffer       = Nothing
                         , surfBufferOffset = 0
                         , surfBufferScale  = 1
                         , surfDamage       = empty
                         , surfOpaque       = everything
                         , surfInput        = everything
                         , surfTransform    = WlOutputTransformNormal
                         , surfChildren     = []
                         }
