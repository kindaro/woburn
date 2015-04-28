{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
module Woburn.Core
where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Data.Int
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Word
import Linear
import Woburn.Backend
import Woburn.Output
import Woburn.Surface
import Woburn.Window

data ClientCallbacks =
    ClientCallbacks { outputAdd :: Output () -> IO ()
                    , outputDel :: OutputId  -> IO ()
                    }

data WoburnState =
    forall s o.
    WoburnState { surfaces :: M.Map SurfaceId (Either SurfaceId (SurfaceSet s))
                , outputs  :: M.Map OutputId (Output o)
                , clients  :: M.Map ClientId (ClientCallbacks, S.Set SurfaceId, S.Set WindowId)
                , backend  :: BackendFunctions s o
                }

newtype Woburn a = Woburn (ReaderT (MVar WoburnState) IO a)
    deriving (Functor, Applicative, Monad, MonadReader (MVar WoburnState), MonadIO)

newtype ClientId = ClientId Word32
    deriving (Eq, Ord, Num, Real, Integral, Enum)

clientCreate :: ClientCallbacks -> Woburn ClientId
clientCreate = undefined

clientTerm :: ClientId -> Woburn ()
clientTerm = undefined

clientCreateSurface :: ClientId -> SurfaceCallbacks -> Woburn SurfaceId
clientCreateSurface = undefined

clientCreateWindow :: ClientId -> WindowCallbacks -> Woburn WindowId
clientCreateWindow = undefined

windowSetTitle :: WindowId -> String -> Woburn ()
windowSetTitle = undefined

windowSetClass :: WindowId -> String -> Woburn ()
windowSetClass = undefined

windowDestroy :: WindowId -> Woburn ()
windowDestroy = undefined

surfaceSetRole :: SurfaceId -> Role -> Woburn ()
surfaceSetRole = undefined

surfaceAttach :: SurfaceId -> Maybe SurfaceId -> Woburn ()
surfaceAttach surf parent = undefined

surfaceDestroy :: SurfaceId -> Woburn ()
surfaceDestroy = undefined

surfaceCommit :: SurfaceId -> SurfaceState -> Woburn ()
surfaceCommit = undefined

surfaceSetPosition :: SurfaceId -> V2 Int32 -> Woburn ()
surfaceSetPosition = undefined

surfaceSetSync :: SurfaceId -> Bool -> Woburn ()
surfaceSetSync = undefined

surfacePlaceAbove :: SurfaceId -> SurfaceId -> Woburn ()
surfacePlaceAbove = undefined

surfacePlaceBelow :: SurfaceId -> SurfaceId -> Woburn ()
surfacePlaceBelow = undefined
