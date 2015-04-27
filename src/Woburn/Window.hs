module Woburn.Window
where

import Data.Int
import Data.Rect

data Window a =
    Window { winRect    :: Rect Int32 -- ^ Window position and size
           , winData    :: a          -- ^ Window data.
           }

instance Functor Window where
    fmap f w = w { winData = f (winData w) }
