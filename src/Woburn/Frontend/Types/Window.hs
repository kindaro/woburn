module Woburn.Frontend.Types.Window
    ( WindowData (..)
    , initialWindowData
    , setState
    , unsetState
    , setSize
    , toWindowState
    )
where

import qualified Data.Set as S
import Data.Int
import Data.Rect
import Data.Word
import Graphics.Wayland
import Linear
import Woburn.Surface hiding (modifyState)
import Woburn.Protocol.XdgShell

data WindowData =
    WindowData { wdSize     :: V2 Word32
               , wdStates   :: S.Set XdgSurfaceState
               , wdObject   :: SObject XdgSurface
               , wdTitle    :: String
               , wdClass    :: String
               , wdGeometry :: Rect Int32
               }
    deriving (Eq, Show)

toWindowState :: WindowData -> WindowState
toWindowState wd =
    WindowState { winTitle    = wdTitle wd
                , winClass    = wdClass wd
                , winGeometry = wdGeometry wd
                , winPopup    = Nothing
                }

initialWindowData :: SObject XdgSurface -> WindowData
initialWindowData obj =
    WindowData { wdSize     = 0
               , wdStates   = S.empty
               , wdObject   = obj
               , wdTitle    = ""
               , wdClass    = ""
               , wdGeometry = Rect 0 maxBound
               }

modifyState :: (S.Set XdgSurfaceState -> S.Set XdgSurfaceState)
            -> WindowData
            -> WindowData
modifyState f w = w { wdStates = f (wdStates w) }

setState :: XdgSurfaceState -> WindowData -> WindowData
setState = modifyState . S.insert

unsetState :: XdgSurfaceState -> WindowData -> WindowData
unsetState = modifyState . S.delete

setSize :: V2 Word32 -> WindowData -> WindowData
setSize sz w = w { wdSize = sz }
