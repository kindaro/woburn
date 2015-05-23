{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Woburn.Surface
    ( SurfaceId
    , Surface (..)
    , SurfaceState (..)
    , SurfaceCallbacks (..)
    , Role (..)
    , Buffered (..)
    , Shuffle (..)
    , ShuffleOperation (..)
    , flipBuffered
    , committed
    , create

{-
    , setSync
    , placeAbove
    , placeBelow
    , delete
    -}
    )
where

import Data.Int
import Data.Function
import Data.Ord
import Data.Region
import Data.Word
import Linear
import Woburn.Output
import Woburn.Protocol

data Buffer = Buffer
    deriving (Eq, Show)

data Role =
    SubSurface
  | ShellSurface
  | Cursor
  deriving (Eq, Show)

data Buffered a = Buffered { currentState :: !a, pendingState :: !a }
    deriving (Eq, Show)

data SurfaceCallbacks =
    SurfaceCallbacks { surfaceFrame :: IO ()
                     , surfaceEnter :: OutputId -> IO ()
                     , surfaceLeave :: OutputId -> IO ()
                     }

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
                 , surfBufferOffset :: V2 Int
                 , surfBufferScale  :: Int
                 , surfDamage       :: Region Int32
                 , surfOpaque       :: Region Int32
                 , surfInput        :: Region Int32
                 , surfTransform    :: WlOutputTransform
                 }
    deriving (Eq, Show)

data Surface s =
    Surface { surfId            :: SurfaceId             -- ^ Unique ID.
            , surfSync          :: Bool                  -- ^ Whether the surface is in sync mode.
            , surfState         :: Maybe SurfaceState    -- ^ The latest commit state.
            , surfPosition      :: Buffered (V2 Int)     -- ^ Surface position relative to parent surface.
            , surfRole          :: Maybe Role            -- ^ The surface role.
            , surfShuffle       :: [Shuffle]             -- ^ Shuffle operations to perform on the next commit.
            , surfCurInput      :: Region Int32          -- ^ Current input region. If this surface is in sync mode
                                                         --   this is copied from the current state when the parent
                                                         --   is committed, otherwise it is updated at the same time
                                                         --   as the current state.
            , surfCallbacks     :: SurfaceCallbacks      -- ^ Surface callback functions.
            , surfBackendData   :: s                     -- ^ Internal data used by the backend.
            }

instance Eq (Surface s) where
    (==) = (==) `on` surfId

instance Ord (Surface s) where
    compare = comparing surfId

instance Show a => Show (Surface a) where
    show s =
        "Surface { surfId           = " ++ show (surfId s) ++ "\n" ++
        "        , surfSync         = " ++ show (surfSync s) ++ "\n" ++
        "        , surfState        = " ++ show (surfState s) ++ "\n" ++
        "        , surfPosition     = " ++ show (surfPosition s) ++ "\n" ++
        "        , surfRole         = " ++ show (surfRole s) ++ "\n" ++
        "        , surfShuffle      = " ++ show (surfShuffle s) ++ "\n" ++
        "        , surfCurInput     = " ++ show (surfCurInput s) ++ "\n" ++
        "        , surfCallbacks    = <callbacks>\n" ++
        "        , surfBackendData  = " ++ show (surfBackendData s) ++ "\n" ++
        "        }\n"

-- | Updates the current value with the pending value, and calculates a new pending value.
flipBuffered :: (a -> a) -> Buffered a -> Buffered a
flipBuffered f b = Buffered { currentState = pendingState b
                            , pendingState = f (pendingState b)
                            }

-- | Updates the surface after it has been committed.
committed :: Surface a -> Surface a
committed surf = surf { surfState    = Nothing
                      , surfCurInput = maybe (surfCurInput surf) surfInput (surfState surf)
                      }

-- | Creates a new surface.
create :: SurfaceCallbacks -> s -> SurfaceId -> Surface s
create cb s sid =
    Surface { surfId            = sid
            , surfSync          = False
            , surfState         = Nothing
            , surfPosition      = Buffered 0 0
            , surfRole          = Nothing
            , surfShuffle       = []
            , surfCurInput      = everything
            , surfCallbacks     = cb
            , surfBackendData   = s
            }
