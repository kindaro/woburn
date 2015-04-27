{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Woburn.Output
    ( Mode (..)
    , Output (..)
    , OutputId
    )
where

import Data.Word
import Woburn.Protocol

data Mode =
    Mode { modeWidth     :: Int  -- ^ Width in hardware units.
         , modeHeight    :: Int  -- ^ Height in hardware units.
         , modeRefresh   :: Int  -- ^ Vertical refresh rate in mHz.
         , modePreferred :: Bool -- ^ Whether this is the preferred mode.
         }
         deriving (Eq, Show, Ord)

newtype OutputId = OutputId Word32
    deriving (Eq, Ord, Num, Real, Integral, Enum)

data Output o =
    Output { outputId          :: OutputId          -- ^ Unique ID number.
           , outputMake        :: String            -- ^ The output make.
           , outputModel       :: String            -- ^ The output model.
           , outputModes       :: [Mode]            -- ^ A list of valid modes.
           , outputPhysWidth   :: Int               -- ^ Physical width in millimeters.
           , outputPhysHeight  :: Int               -- ^ Physical height in millimeters.
           , outputSubpixel    :: WlOutputSubpixel  -- ^ Subpixel orientation.
           , outputTransform   :: WlOutputTransform -- ^ Transform mapping framebuffer to output.
           , outputScale       :: Int               -- ^ Scaling factor.
           , outputBackendData :: o                 -- ^ Backend specific data.
           }

instance Functor Output where
    fmap f o = o { outputBackendData = f (outputBackendData o) }
