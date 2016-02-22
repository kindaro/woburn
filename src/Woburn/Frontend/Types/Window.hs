module Woburn.Frontend.Types.Window
    ( WindowData (..)
    , WindowsData (..)
    , initialWindowsData
    , setState
    , unsetState
    , setSize
    , lookup
    )
where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.Word
import Linear
import Prelude hiding (lookup)
import Woburn.Protocol.XdgShell
import Woburn.Window

data WindowData =
    WindowData { wdSize   :: V2 Word32
               , wdStates :: S.Set XdgSurfaceState
               }
    deriving (Eq, Show)

data WindowsData = WindowsData { windows :: M.Map WindowId WindowData }
    deriving (Eq, Show)

initialWindowsData :: WindowsData
initialWindowsData = WindowsData { windows = M.empty }

initialWindowData = WindowData 0 S.empty

modifyWindow :: (WindowData -> WindowData)
             -> WindowId
             -> WindowsData
             -> WindowsData
modifyWindow f wid wd =
    wd { windows = M.alter (Just . f . fromMaybe initialWindowData) wid (windows wd) }

modifyState :: (S.Set XdgSurfaceState -> S.Set XdgSurfaceState)
            -> WindowId
            -> WindowsData
            -> WindowsData
modifyState f = modifyWindow (\w -> w { wdStates = f (wdStates w) })

setState :: XdgSurfaceState -> WindowId -> WindowsData -> WindowsData
setState = modifyState . S.insert

unsetState :: XdgSurfaceState -> WindowId -> WindowsData -> WindowsData
unsetState = modifyState . S.delete

setSize :: V2 Word32 -> WindowId -> WindowsData -> WindowsData
setSize size = modifyWindow (\w -> w { wdSize = size })

lookup :: WindowId -> WindowsData -> WindowData
lookup wid = fromMaybe initialWindowData . M.lookup wid . windows
