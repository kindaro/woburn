{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Woburn.Surface
    ( SurfaceId
    , Surface (..)
    , SurfaceState (..)
    , Buffered (..)
    , Shuffle (..)
    , ShuffleOperation (..)
    , commitPosition
    , getPosition
    , setPosition
    , committed
    , create
    )
where

import Data.Int
import Data.Region
import Data.Word
import Linear
import Woburn.Protocol

data Buffer = Buffer
    deriving (Eq, Show)

data Buffered a = Buffered { currentState :: !a, pendingState :: !a }
    deriving (Eq, Show)

newtype SurfaceId = SurfaceId Word32
    deriving (Eq, Ord, Show, Num, Real, Integral, Enum)

data Shuffle = Shuffle ShuffleOperation SurfaceId SurfaceId
    deriving (Eq, Show)

data ShuffleOperation =
    PlaceAbove
  | PlaceBelow
  | DeletedAbove
  | DeletedBelow
  deriving (Eq, Show)

data SurfaceState =
    SurfaceState { surfBuffer       :: Maybe Buffer
                 , surfBufferOffset :: V2 Int32
                 , surfBufferScale  :: Int
                 , surfDamage       :: Region Int32
                 , surfOpaque       :: Region Int32
                 , surfInput        :: Region Int32
                 , surfTransform    :: WlOutputTransform
                 }
    deriving (Eq, Show)

data Surface s =
    Surface { surfSync      :: Bool                  -- ^ Whether the surface is in sync mode.
            , surfState     :: Maybe SurfaceState    -- ^ The pending state that will be used on the next commit.
            , surfPosition  :: Buffered (V2 Int32)   -- ^ Surface position relative to parent surface.
            , surfShuffle   :: [Shuffle]             -- ^ Shuffle operations to perform on the next commit.
            , surfCurInput  :: Region Int32          -- ^ Current input region. If this surface is in sync mode
                                                     --   this is copied from the current state when the parent
                                                     --   is committed, otherwise it is updated at the same time
                                                     --   as the current state.
            , surfData      :: s                     -- ^ Internal data used by the backend.
            }
    deriving (Eq)

instance Show a => Show (Surface a) where
    show s =
        "Surface { surfSync     = " ++ show (surfSync s) ++ "\n" ++
        "        , surfState    = " ++ show (surfState s) ++ "\n" ++
        "        , surfPosition = " ++ show (surfPosition s) ++ "\n" ++
        "        , surfShuffle  = " ++ show (surfShuffle s) ++ "\n" ++
        "        , surfCurInput = " ++ show (surfCurInput s) ++ "\n" ++
        "        , surfData     = " ++ show (surfData s) ++ "\n" ++
        "        }\n"

-- | Updates the current value with the pending value.
flipBuffered :: Buffered a -> Buffered a
flipBuffered b = b { currentState = pendingState b }

-- | Commits the pending position.
commitPosition :: Surface a -> Surface a
commitPosition s = s { surfPosition = flipBuffered (surfPosition s) }

-- | Sets the pending position.
setPosition :: V2 Int32 -> Surface a -> Surface a
setPosition pos s = s { surfPosition = (surfPosition s) { pendingState = pos } }

-- | Gets the current position.
getPosition :: Surface a -> V2 Int32
getPosition = currentState . surfPosition

-- | Updates the surface after it has been committed.
--
-- Removes the pending state after updating the current input region it.
committed :: Surface a -> Surface a
committed surf = surf { surfState    = Nothing
                      , surfCurInput = maybe (surfCurInput surf) surfInput (surfState surf)
                      }

-- | Creates a new surface.
create :: s -> Surface s
create s =
    Surface { surfSync      = False
            , surfState     = Nothing
            , surfPosition  = Buffered 0 0
            , surfShuffle   = []
            , surfCurInput  = everything
            , surfData      = s
            }
