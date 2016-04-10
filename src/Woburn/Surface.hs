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
import Data.Monoid
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
                 , surfChildren     :: ([a], [a])
                 }
    deriving (Eq, Show)

instance Functor SurfaceState where
    fmap f s =
        let (l, r) = surfChildren s
        in s { surfChildren = (map f l, map f r) }

instance Foldable SurfaceState where
    foldMap f SurfaceState { surfChildren = (l, r) } =
        foldMap f l <> foldMap f r

instance Traversable SurfaceState where
    traverse f s@SurfaceState { surfChildren = (l, r) } =
        (\a b -> s { surfChildren = (a, b) })
        <$> traverse f l
        <*> traverse f r

data Surface s a =
    Surface { surfState :: SurfaceState a -- ^ The current surface state.
            , surfData  :: s              -- ^ Internal data used by the backend.
            }
    deriving (Eq, Show)

instance Functor (Surface s) where
    fmap f s = s { surfState = fmap f (surfState s) }

instance Foldable (Surface s) where
    foldMap f s = foldMap f (surfState s)

instance Traversable (Surface s) where
    traverse f s = (\st -> s { surfState = st }) <$> traverse f (surfState s)

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
                         , surfChildren     = ([], [])
                         }
