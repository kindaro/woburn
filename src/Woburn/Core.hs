{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Woburn.Core
    ( Request (..)
    , Event (..)
    , Error
    , handleInput
    , newCoreState

    -- * Core input commands.
    , CoreInput (..)

    -- * Core output.
    , CoreOutputF (..)
    )
where

import Control.Applicative
import Control.Arrow
import Control.Monad.Free
import Control.Monad.Free.TH
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens hiding (universe)
import Data.Foldable
import Data.Maybe
import Data.Int
import Data.Rect
import Data.Tuple
import Data.Traversable (mapAccumR)
import qualified Data.Map as M
import Data.Word
import Linear

import Prelude

import qualified Woburn.Backend as B
import Woburn.Buffer
import qualified Woburn.Layout as L
import Woburn.Output
import Woburn.Protocol.Core
import Woburn.Surface
import qualified Woburn.Surface.Map as SM
import qualified Woburn.Universe as U
import Woburn.Types

data ClientSurfaceId = ClientSurfaceId ClientId SurfaceId
    deriving (Eq, Ord, Show)

data CoreState s =
    CoreState { outputs  :: [(OutputId, MappedOutput)]
              , clients  :: M.Map ClientId (ClientData s)
              , universe :: U.Universe ClientSurfaceId
              , layout   :: L.Layout (ClientSurfaceId, [(V2 Int32, Surface s ())])
              }

data ClientData s =
    ClientData { surfaces :: SM.SurfaceMap s }

-- | A core request.
data Request =
    SurfaceCreate SurfaceId
  | SurfaceDestroy SurfaceId
  | SurfaceCommit [(SurfaceId, SurfaceState (V2 Int32, SurfaceId))]
  deriving (Eq, Show)

-- | A core event.
data Event =
    OutputAdded OutputId MappedOutput
  | OutputRemoved OutputId
  | SurfaceFrame [SurfaceId]
  | BufferReleased Buffer
  | WindowConfigure SurfaceId (V2 Word32)
  | Error Error
  deriving (Eq, Show)

-- | A core error.
data Error =
    BadSurface
  | BadWindow
  deriving (Eq, Show)

data CoreInput s =
    ClientAdd ClientId
  | ClientDel ClientId
  | ClientRequest ClientId Request
  | BackendEvent B.Event
  deriving (Eq, Show)

data CoreOutputF s a =
    ClientEvent (Maybe ClientId) Event a
  | BackendRequest (B.Request s) a
  | BackendSurfGet (s -> a)
  | CoreError String a
  deriving (Functor)

$(makeFree ''CoreOutputF)

-- | Returns the size of an 'Output'.
outputSize :: Output -> V2 Word32
outputSize o
    | isPortrait = V2 h w
    | otherwise  = V2 w h
    where
        m = outputCurMode o
        s = outputScale o
        w = modeWidth  m `div` s
        h = modeHeight m `div` s
        -- Checks if the output is in portrait mode.
        isPortrait = case outputTransform o of
                          WlOutputTransformNormal     -> False
                          WlOutputTransform90         -> True
                          WlOutputTransform180        -> False
                          WlOutputTransform270        -> True
                          WlOutputTransformFlipped    -> False
                          WlOutputTransformFlipped90  -> True
                          WlOutputTransformFlipped180 -> False
                          WlOutputTransformFlipped270 -> True

-- | Maps an output at a given X-offset into the global compositor space.
mapOutput :: Word32 -> Output -> MappedOutput
mapOutput off out = MappedOutput out . shiftX off $ outRect out
    where
        outRect = Rect 0 . fmap (subtract 1) . outputSize

-- | Gives a list of 'Output's positions in the global compositor space.
--
-- The first 'Output' in the list will be the right-most 'Output'.
mapOutputs :: [(OutputId, Output)] -> [(OutputId, MappedOutput)]
mapOutputs = snd . mapAccumR f 0
    where
        f off (oid, out) =
            let out'@(MappedOutput _ r) = mapOutput off out
            in (off + width r, (oid, out'))

-- | Commits a list of surfaces to the backend.
backendCommit :: (MonadState (CoreState s) m, MonadFree (CoreOutputF s) m) => [Surface s ()] -> m ()
backendCommit ss = do
    l  <- gets layout
    backendRequest . B.SurfaceCommit ss . L.toList $ fmap snd l

-- | Handles backend events.
handleBackendEvent :: (MonadState (CoreState s) m, MonadFree (CoreOutputF s) m) => B.Event -> m ()
handleBackendEvent evt =
    case evt of
         B.BufferReleased buf -> clientEvent (Just $ bufClientId buf) (BufferReleased buf)

         B.OutputAdded outId out -> do
             mOut <- state $ \s ->
                 let outs =
                        mapOutputs
                        . (++ [(outId, out)])
                        . map (second mappedOutput)
                        . filter ((/= outId) . fst)
                        $ outputs s
                 in
                 (lookup outId outs, s { outputs = outs })
             clientEvent Nothing (OutputAdded outId (fromJust mOut))
             setUniverse $ U.setOutputs <$> gets outputs <*> gets universe

         B.OutputRemoved oid -> do
             modify $ \s -> s { outputs = filter ((/= oid) . fst) (outputs s) }
             clientEvent Nothing (OutputRemoved oid)
             setUniverse $ U.setOutputs <$> gets outputs <*> gets universe

         B.OutputFrame oid -> do
             wins <- gets (U.onOutput oid . universe)
             cs   <- gets clients
             mapM_
                (\(cid, sids) -> clientEvent (Just cid) (SurfaceFrame sids))
                (mapMaybe (mapWindowToSurfaces cs) wins)

         B.Input ts is -> return ()
    where
        -- | Maps a window to its list of surfaces.
        mapWindowToSurfaces cs (ClientSurfaceId cid sid) = do
            cd  <- M.lookup cid cs
            return (cid, SM.lookupAllIds sid (surfaces cd))

-- | Sends a configure event for a single window.
configureWindow :: MonadFree (CoreOutputF s) m
                => V2 Word32        -- ^ The window size.
                -> ClientSurfaceId  -- ^ The window ID.
                -> m ()
configureWindow sz (ClientSurfaceId cid sid) =
    clientEvent (Just cid) (WindowConfigure sid sz)

-- | Sets the universe, and recomputes the layout.
setUniverse :: (MonadState (CoreState s) m, MonadFree (CoreOutputF s) m)
            => m (U.Universe ClientSurfaceId)
            -> m ()
setUniverse f = do
    uni       <- f
    oldLayout <- gets layout

    let newLayoutWindows = L.fromUniverse uni
    newLayoutSurfaces <- traverse (runKleisli $ returnA &&& Kleisli lookupAllClientSurface) newLayoutWindows

    modify $ \s -> s { universe = uni, layout = newLayoutSurfaces }
    mapM_ (uncurry configureWindow . first size) (L.difference (fmap fst oldLayout) newLayoutWindows)

-- | Looks up a window and all its surfaces.
lookupAllClientSurface :: (MonadState (CoreState s) m, MonadFree (CoreOutputF s) m)
                       => ClientSurfaceId
                       -> m [(V2 Int32, Surface s ())]
lookupAllClientSurface csid@(ClientSurfaceId cid sid) =
    (maybe [] (lookupWindow . surfaces) . M.lookup cid) <$> gets clients
    where
        lookupWindow surfs =
            let winOffset =
                    maybe 0 (topLeft . winGeometry) $
                        SM.lookup sid surfs >>= surfWindowState . surfState
             in SM.lookupAll (-winOffset) sid surfs

modifyUniverse :: (MonadState (CoreState s) m, MonadFree (CoreOutputF s) m)
               => (U.Universe ClientSurfaceId -> U.Universe ClientSurfaceId) -> m ()
modifyUniverse f = setUniverse (f <$> gets universe)

handleCoreRequest :: (MonadState (CoreState s) m, MonadFree (CoreOutputF s) m) => ClientId -> Request -> m ()
handleCoreRequest cid req =
    case req of
         SurfaceCreate      sid      -> do
             surf <- create <$> backendSurfGet
             modifySurfaces $ SM.insert sid surf
         SurfaceDestroy     sid      -> do
             msurf <- lookupSurface sid
             modifyUniverse . U.delete $ ClientSurfaceId cid sid
             backendCommit []
             modifySurfaces $ SM.delete sid
             maybe (return ()) (backendRequest . B.SurfaceDestroy . (: []) . void) msurf
         SurfaceCommit sids -> do
             (ops, surfs) <- (unzip . catMaybes) <$> mapM
                (\(sid, ss) ->
                    state $ \s ->
                        maybe (Nothing, s) (first Just) $ do
                            cd   <- M.lookup cid (clients s)
                            surf <- SM.lookup sid (surfaces cd)

                            let surf' = surf { surfState = ss }
                                cd'   = cd { surfaces = SM.insert sid surf' (surfaces cd) }
                                s'    = s { clients = M.insert cid cd' (clients s) }

                            return ((uniOp sid ss (surfState surf), void surf'), s')
                )
                sids
             modifyUniverse $ foldr (.) id ops
             backendCommit surfs
    where
        -- Remove or add the surface as a window if it is (un)mapped.
        uniOp sid ss oldSs =
            case (isMapped oldSs, isMapped ss) of
              (True , False) -> U.delete $ ClientSurfaceId cid sid
              (False, True ) -> U.insert $ ClientSurfaceId cid sid
              _              -> id

        modifyClient f = modify $ \s -> s { clients = M.adjust f cid (clients s) }
        modifySurfaces f = modifyClient $ \c -> c { surfaces = f (surfaces c) }
        lookupSurface sid = gets $ \s -> do
            cd <- M.lookup cid (clients s)
            SM.lookup sid (surfaces cd)

announceOutputs :: (MonadState (CoreState s) m, MonadFree (CoreOutputF s) m) => ClientId -> m ()
announceOutputs cid = do
    os <- gets outputs
    mapM_ (clientEvent (Just cid) . uncurry OutputAdded) os

-- | Handles the various core inputs.
handleInput :: (MonadState (CoreState s) m, MonadFree (CoreOutputF s) m) => CoreInput s -> m ()
handleInput input =
    case input of
         BackendEvent  evt     -> handleBackendEvent evt
         ClientRequest cid req -> handleCoreRequest cid req
         ClientAdd     cid     -> do
             modify (\s -> s { clients = M.insert cid newClientData (clients s) })
             announceOutputs cid
         ClientDel     cid     -> do
             -- When a client is deleted we have to remove the client data
             cd <- state $ \s -> (M.lookup cid (clients s), s { clients  = M.delete cid (clients s) })
             -- and all of its windows
             modifyUniverse . U.filter $ \(ClientSurfaceId cid' _) -> cid' /= cid
             backendCommit []
             -- and destoy its surfaces
             backendRequest . B.SurfaceDestroy $ maybe [] (map void . SM.elems . surfaces) cd
    where
        newClientData = ClientData SM.empty

-- | Creates a new 'CoreState'.
newCoreState :: CoreState s
newCoreState =
    CoreState { outputs  = []
              , clients  = M.empty
              , universe = U.create ["workspace"]
              , layout   = L.empty
              }
