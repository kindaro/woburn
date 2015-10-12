{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Woburn.Window
    ( Window (..)
    , WindowId
    )
where

import Data.Word
import Woburn.Surface

newtype WindowId = WindowId Word32
    deriving (Show, Eq, Ord, Num, Real, Integral, Enum)

data Window a =
    Window { winTitle     :: String             -- ^ Window title.
           , winClass     :: String             -- ^ Window class.
           , winSurface   :: SurfaceId          -- ^ Root surface.
           }
    deriving (Eq, Show)
