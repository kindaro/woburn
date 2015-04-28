{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Woburn.Window
    ( Window (..)
    , WindowId
    , WindowCallbacks (..)
    )
where

import Data.Int
import Data.Word
import Data.Rect
import Linear

data WindowCallbacks =
    WindowCallbacks { windowConfigure :: Word32 -> V2 Int32 -> IO ()
                    }

newtype WindowId = WindowId Word32
    deriving (Eq, Ord, Num, Real, Integral, Enum)

data Window a =
    Window { winId        :: WindowId           -- ^ Unique window id.
           , winRect      :: Rect Int32         -- ^ Window position and size.
           , winTitle     :: String             -- ^ Window title.
           , winClass     :: String             -- ^ Window class.
           , winCallbacks :: WindowCallbacks    -- ^ Window callbacks.
           , winData      :: a                  -- ^ Window data.
           }

instance Functor Window where
    fmap f w = w { winData = f (winData w) }
