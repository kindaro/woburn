{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TupleSections #-}
module Woburn.Core
    ( Request
    , Event
    , Error
    , ClientId
    , run
    )
where

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens hiding (universe)
import Data.Foldable (find)
import Data.Maybe
import Data.Int
import Data.Rect
import Data.STree
import Data.Tuple
import Data.Traversable (mapAccumR)
import qualified Data.Map as M
import qualified Data.Set.Diet as D
import Data.Word
import Linear

import Pipes
import qualified Pipes.Concurrent as PC
import qualified Pipes.Prelude as P
import Prelude

import qualified Woburn.Backend as B
import Woburn.Layout
import Woburn.Output
import Woburn.Protocol
import Woburn.Surface
import Woburn.Surface.Tree
import Woburn.Window
import qualified Woburn.Universe as U

data CoreState s =
    CoreState { outputs  :: [MappedOutput]
              , clients  :: M.Map ClientId (ClientData s)
              , ids      :: D.Diet Word32
              , universe :: U.Universe WindowId
              , layedOut :: [(MappedOutput, [(Rect Word32, WindowId)])]
              }

data CoreData s =
    CoreData { backendRequest :: PC.Output (B.Request s)
             , backendSurfGet :: IO s
             }

data ClientData s =
    ClientData { surfaces :: M.Map SurfaceId (Surface s, Either SurfaceId (STree SurfaceId))
               , windows  :: M.Map WindowId ()
               , output   :: PC.Output Event
               }

type Core s a = ReaderT (CoreData s) (StateT (CoreState s) IO) a

runCore :: CoreData s -> CoreState s -> Core s a -> IO a
runCore cd cs c = evalStateT (runReaderT c cd) cs

newtype ClientId = ClientId Word32
    deriving (Eq, Ord, Num, Real, Integral, Enum)

data Request =
    WindowCreate WindowId SurfaceId
  | WindowDestroy WindowId
  | WindowSetTitle WindowId String
  | WindowSetClass WindowId String
  | SurfaceCreate SurfaceId
  | SurfaceDestroy SurfaceId
  | SurfaceAttach SurfaceId (Maybe SurfaceId)
  | SurfaceCommit SurfaceId SurfaceState
  | SurfaceSetPosition SurfaceId (V2 Int32)
  | SurfaceSetSync SurfaceId Bool
  | SurfacePlaceAbove SurfaceId SurfaceId
  | SurfacePlaceBelow SurfaceId SurfaceId
  deriving (Eq, Show)

data Event =
    OutputAdded MappedOutput
  | OutputRemoved MappedOutput
  | Error Error
  deriving (Eq, Show)

data Error =
    BadSurface SurfaceId
  | BadShuffle SurfaceId SurfaceId
  | BadWindow WindowId
  deriving (Eq, Show)

data Message =
    ClientAdd ClientId (PC.Output Event)
  | ClientDel ClientId
  | ClientRequest ClientId Request
  | BackendEvent B.Event

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
mapOutputs :: Word32 -> [Output] -> [MappedOutput]
mapOutputs start = snd . mapAccumR f start
    where
        f off out =
            let out'@(MappedOutput _ r) = mapOutput off out
            in (off + width r, out')

-- | Returns the right-most edge of a list of outputs, assuming the first
-- element is the right-most one.
outputsRight :: [MappedOutput] -> Word32
outputsRight []    = 0
outputsRight (o:_) = mappedRect o ^. to bottomRight . _x . to (+ 1)

-- | Deletes an output from a list of mapped outputs, and returns the deleted
-- item (or 'Nothing' if it was not in the list), along with a list of the
-- other outputs remapped to fill the hole of the removed output.
deleteOutput :: OutputId -> [MappedOutput] -> (Maybe MappedOutput, [MappedOutput])
deleteOutput oid os =
    let (as, bs) = span ((/= oid) . outputId . mappedOutput) os
    in
    case bs of
         []     -> (Nothing, as)
         (x:xs) -> (Just x, mapOutputs (outputsRight xs) (map mappedOutput as) ++ xs)

-- | Updates the layout.
--
-- Should be called whenever the 'universe' has changed.
doLayout :: Core s ()
doLayout = modify $ \s -> s { layedOut = layout (universe s) }

-- | Handles backend events.
handleBackendEvent :: B.Event -> Core s ()
handleBackendEvent evt = do
    case evt of
         B.OutputAdded   out -> do
             mOut <- state $ \s ->
                 let outs = snd . deleteOutput (outputId out) $ outputs s
                     mOut = mapOutput (outputsRight outs) out
                 in
                 (mOut, s { outputs = mOut : outs })
             sendClientEvent Nothing (OutputAdded mOut)
         B.OutputRemoved oid -> do
             mOut <- state $ \s -> second (\x -> s { outputs = x}) . deleteOutput oid $ outputs s
             case mOut of
                  Nothing  -> error "Backend removed a non-existing output"
                  Just out -> sendClientEvent Nothing (OutputRemoved out)
    modify $ \s -> s { universe = U.setOutputs (outputs s) (universe s) }
    doLayout

handleCoreRequest :: ClientId -> Request -> Core s ()
handleCoreRequest cid req =
    case req of
         WindowCreate       wid sid   -> undefined
         WindowDestroy      wid       -> undefined
         WindowSetTitle     wid title -> undefined
         WindowSetClass     wid cls   -> undefined
         SurfaceCreate      sid       -> undefined
         SurfaceDestroy     sid       -> undefined
         SurfaceAttach      sid tid   -> undefined
         SurfaceCommit      sid ss    -> undefined
         SurfaceSetPosition sid pos   -> undefined
         SurfaceSetSync     sid sync  -> undefined
         SurfacePlaceAbove  sid tid   -> undefined
         SurfacePlaceBelow  sid tid   -> undefined

handleMsg :: Message -> Core s ()
handleMsg msg =
    case msg of
         BackendEvent evt      -> handleBackendEvent evt
         ClientRequest cid req -> handleCoreRequest cid req

-- | Runs an effect, and links it to the current thread.
-- The 'link' ensures any exceptions thrown while running the effect, is
-- re-thrown in the current thread.
runAndLink :: Effect IO () -> IO ()
runAndLink effect = async (runEffect effect) >>= link

-- TODO: Ignore sends to exhausted outputs?
sendToOutput :: MonadIO m => a -> PC.Output a -> m ()
sendToOutput val out = liftIO (PC.atomically (PC.send out val)) >>= checkRet
    where
        checkRet True  = return ()
        checkRet False = error "Trying to send to an exhausted output"

sendBackendRequest :: B.Request s -> Core s ()
sendBackendRequest req = asks backendRequest >>= sendToOutput req

-- | Sends an event to one or all clients.
--
-- If passed 'Nothing' as the first argument, the event is sent to all clients,
-- otherwise it is only sent to the specified client.
--
-- Trying to send an event to a client that does not exist results in an error.
sendClientEvent :: Maybe ClientId -> Event -> Core s ()
sendClientEvent Nothing evt =
    gets clients >>=
        mapM_ (sendToOutput evt . output) . M.elems
sendClientEvent (Just cid) evt =
    gets clients >>=
        maybe
            (error "Trying to send an event to an unknown client")
            (sendToOutput evt . output) . M.lookup cid

-- | Creates an IO action that will cause a backend-specific surface to be
-- pulled from the backend, and returned.
createSurfGetter :: Producer s IO () -> IO (IO s)
createSurfGetter pSurf = do
    sem <- newQSem 0
    var <- newEmptyMVar
    a   <- async . runEffect $ pSurf >-> putS sem var
    link a
    return $ getS sem var
    where
        putS sem var = do
            liftIO $ waitQSem sem
            s <- await
            liftIO $ putMVar var s
        getS sem var = do
            signalQSem sem
            readMVar var

-- | Returns a producer of messages.
--
-- Passes on backend events, and creates threads to handle new clients.
msgGenerator :: MonadIO m
             => Producer (Consumer Event IO (), Producer Request IO ()) IO () -- ^ New clients.
             -> Producer B.Event IO ()      -- ^ Incoming backend events.
             -> IO (Producer Message m ())
msgGenerator clients bEvt = do
    (msgOut, msgIn) <- PC.spawn PC.unbounded
    dVar            <- newMVar D.empty

    runAndLink . for clients $ lift . \(evt, req) -> do
        -- Pass events from the event buffer to the client
        (evtOut, evtIn) <- PC.spawn PC.unbounded
        runAndLink $ PC.fromInput evtIn >-> evt

        -- Create client ID, and tell the core about it
        cid <- modifyMVar dVar (return . swap . fromMaybe (error "Ran out of client IDs!") . D.minView)
        _   <- PC.atomically . PC.send msgOut $ ClientAdd cid evtOut

        -- Fetch client requests until the pipe is exhausted.
        runAndLink $ do
            req >-> P.map (ClientRequest cid) >-> PC.toOutput msgOut

            -- Client is done, remove it
            lift . void . PC.atomically . PC.send msgOut $ ClientDel cid
            lift . modifyMVar_ dVar $ return . D.insert cid

    runAndLink $ bEvt >-> P.map BackendEvent >-> PC.toOutput msgOut

    return $ PC.fromInput msgIn

-- | Runs the core.
run :: Consumer (B.Request s) IO () -- ^ Output requests to the backend.
    -> Producer B.Event IO ()       -- ^ Incoming events from the backend.
    -> Producer s IO ()             -- ^ A producer of backend surface data.
    -> Producer (Consumer Event IO (), Producer Request IO ()) IO () -- ^ New client connections.
    -> IO ()
run bReq bEvt bSurf clients = do
    msg     <- msgGenerator clients bEvt
    surfGet <- createSurfGetter bSurf

    (bReqOutput, bReqInput) <- PC.spawn PC.unbounded
    runAndLink $ PC.fromInput bReqInput >-> bReq

    let cd = CoreData { backendRequest = bReqOutput
                      , backendSurfGet = surfGet
                      }
        cs = CoreState { outputs  = []
                       , clients  = M.empty
                       , ids      = D.empty
                       , universe = U.create ["workspace"]
                       , layedOut = []
                       }

    runCore cd cs . runEffect . for msg $ lift . handleMsg
