{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Woburn.Output
    ( Mode (..)
    , Output (..)
    , OutputId
    , MappedOutput (..)
    )
where

import Data.Rect
import Data.Word
import Woburn.Protocol

data Mode =
    Mode { modeWidth     :: Word32 -- ^ Width in hardware units.
         , modeHeight    :: Word32 -- ^ Height in hardware units.
         , modeRefresh   :: Word32 -- ^ Vertical refresh rate in mHz.
         , modePreferred :: Bool   -- ^ Whether this is the preferred mode.
         }
         deriving (Eq, Show, Ord)

newtype OutputId = OutputId Word32
    deriving (Show, Eq, Ord, Num, Real, Integral, Enum)

data Output =
    Output { outputId          :: OutputId          -- ^ Unique ID number.
           , outputMake        :: String            -- ^ The output make.
           , outputModel       :: String            -- ^ The output model.
           , outputCurMode     :: Mode              -- ^ The current mode.
           , outputModes       :: [Mode]            -- ^ Other available modes.
           , outputPhysWidth   :: Word32            -- ^ Physical width in millimeters.
           , outputPhysHeight  :: Word32            -- ^ Physical height in millimeters.
           , outputSubpixel    :: WlOutputSubpixel  -- ^ Subpixel orientation.
           , outputTransform   :: WlOutputTransform -- ^ Transform mapping framebuffer to output.
           , outputScale       :: Word32            -- ^ Scaling factor.
           }
    deriving (Eq, Show, Ord)

data MappedOutput =
    MappedOutput { mappedOutput :: Output
                 , mappedRect   :: Rect Word32
                 }
    deriving (Eq, Show)
