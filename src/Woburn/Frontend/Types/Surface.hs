module Woburn.Frontend.Types.Surface
    ( SurfaceData (..)
    , SurfacesData (..)
    , surfaceToId
    , insertSurface
    , deleteSurface
    , adjustSurface
    , lookupSurface
    , insertCallbacks
    , lookupCallbacks
    , deleteCallbacks
    , initialSurfaceData
    , initialSurfacesData
    , commit
    -- * Functions used for sub surfaces.
    , addSubsurface
    , destroySubsurface
    , setPosition
    , placeAbove
    , placeBelow
    , setSync
    )
where

import Control.Arrow
import Control.Monad.State
import Data.Function
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.Region as R
import qualified Data.Map as M
import Graphics.Wayland
import Linear
import Woburn.Buffer
import Woburn.Protocol.Core
import Woburn.Surface

-- | Convert from a 'WlSurface' object to its 'SurfaceId'.
surfaceToId :: SObject WlSurface -> SurfaceId
surfaceToId = fromIntegral . unObjId . unObject

-- | Converts from 'SurfaceData' to a 'SurfaceState' that can be committed.
toSurfaceState :: SurfaceData -> SurfaceState (V2 Int32, SurfaceId)
toSurfaceState sd =
    SurfaceState { surfBuffer       = sdBuffer sd
                 , surfBufferOffset = sdBufferOffset sd
                 , surfBufferScale  = sdBufferScale sd
                 , surfDamage       = combinedDamage
                 , surfOpaque       = sdOpaque sd
                 , surfInput        = sdInput sd
                 , surfTransform    = sdBufferTransform sd
                 , surfChildren     = uncurry ((,) `on` map (second surfaceToId)) (sdChildren sd)
                 }
    where
        combinedDamage =
            sdDamageBuffer sd
            `R.union`
            R.scale (sdBufferScale sd) (R.offset (- sdBufferOffset sd) (sdDamageSurface sd))

-- | Per-surface data.
data SurfaceData =
    SurfaceData { sdDamageSurface   :: R.Region Int32
                , sdDamageBuffer    :: R.Region Int32
                , sdOpaque          :: R.Region Int32
                , sdInput           :: R.Region Int32
                , sdBuffer          :: Maybe Buffer
                , sdBufferOffset    :: V2 Int32
                , sdBufferTransform :: WlOutputTransform
                , sdBufferScale     :: Int32
                , sdFrameCallbacks  :: [SObject WlCallback]
                , sdSubsurface      :: Maybe (SObject WlSubsurface)
                , sdChildren        :: ([(V2 Int32, SObject WlSurface)], [(V2 Int32, SObject WlSurface)])
                , sdSync            :: Bool
                , sdInheritedSync   :: Bool
                , sdCached          :: Maybe (SurfaceState (V2 Int32, SurfaceId))
                }

-- | Data for all the surfaces.
data SurfacesData =
    SurfacesData { surfaces  :: M.Map (SObject WlSurface) SurfaceData
                 , callbacks :: M.Map SurfaceId [SObject WlCallback]
                 }

insertSurface :: SObject WlSurface -> SurfaceData -> SurfacesData -> SurfacesData
insertSurface surf dat sd = sd { surfaces = M.insert surf dat (surfaces sd) }

deleteSurface :: SObject WlSurface -> SurfacesData -> SurfacesData
deleteSurface surf sd = sd { surfaces = M.delete surf (surfaces sd) }

adjustSurface :: (SurfaceData -> SurfaceData) -> SObject WlSurface -> SurfacesData -> SurfacesData
adjustSurface f surf sd = sd { surfaces = M.adjust f surf (surfaces sd) }

lookupSurface :: SObject WlSurface -> SurfacesData -> Maybe SurfaceData
lookupSurface obj = M.lookup obj . surfaces

insertCallbacks :: SurfaceId -> [SObject WlCallback] -> SurfacesData -> SurfacesData
insertCallbacks sid cbs sd = sd { callbacks = M.insertWith (++) sid cbs (callbacks sd) }

lookupCallbacks :: SurfaceId -> SurfacesData -> [SObject WlCallback]
lookupCallbacks sid = fromMaybe [] . M.lookup sid . callbacks

deleteCallbacks :: SurfaceId -> SurfacesData -> SurfacesData
deleteCallbacks sid sd = sd { callbacks = M.delete sid (callbacks sd) }

-- | Adds a new sub-surface.
addSubsurface :: SObject WlSurface -> SObject WlSubsurface -> SObject WlSurface -> SurfacesData -> SurfacesData
addSubsurface surf subsurf parent sd = fromMaybe sd $ do
    ps <- lookupSurface parent sd
    return .
        adjustSurface (\s -> s { sdChildren = first ((0, surf) :) (sdChildren s) }) parent
        $ adjustSurface (\s -> s { sdSubsurface    = Just subsurf
                                 , sdSync          = True
                                 , sdInheritedSync = sdSync ps
                                 }) surf sd

destroySubsurface :: SObject WlSurface -> SObject WlSurface -> SurfacesData -> SurfacesData
destroySubsurface surf parent =
    adjustSurface (\s -> s { sdChildren = filterChildren (sdChildren s) }) parent
    . adjustSurface (\s -> s { sdSubsurface = Nothing }) surf
    where
        filterChildren = uncurry ((,) `on` filter ((/= surf) . snd))

-- | Applies a function to all elements that fulfills a predicate.
mapPred :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapPred _ _ []     = []
mapPred p f (x:xs)
    | p x       = f x : mapPred p f xs
    | otherwise = x   : mapPred p f xs

-- | Sets the position of a sub-surface relative to its parent.
setPosition :: SObject WlSurface -> V2 Int32 -> SObject WlSurface -> SurfacesData -> SurfacesData
setPosition surf pos =
    adjustSurface
    (\s -> s { sdChildren = uncurry ((,) `on` mapPred ((== surf) . snd) (first $ const pos)) (sdChildren s) })

data ShuffleOp = Above | Below

shuffle :: SObject WlSurface
        -> SObject WlSurface
        -> SObject WlSurface
        -> ShuffleOp
        -> SurfaceData
        -> SurfaceData
shuffle a b p op surf =
    case partition ((== a) . snd) (toList surf) of
      ([ea], rest) -> surf { sdChildren = fromList $ concatMap (f ea) rest }
      _            -> surf
    where
        f ea x =
            case (snd x == b, op) of
              (True, Above) -> [ea, x]
              (True, Below) -> [x, ea]
              _             -> [x]

        toList s =
            let (l, r) = sdChildren s
             in l ++ [(0, p)] ++ r

        fromList ls =
            let (l, _ : r) = span ((/= p) . snd) ls
             in (l, r)

placeAbove :: SObject WlSurface -> SObject WlSurface -> SObject WlSurface -> SurfacesData -> SurfacesData
placeAbove a b p = adjustSurface (shuffle a b p Above) p

placeBelow :: SObject WlSurface -> SObject WlSurface -> SObject WlSurface -> SurfacesData -> SurfacesData
placeBelow a b p = adjustSurface (shuffle a b p Below) p

-- | Commits a surface (if it is not in sync-mode) and all sub-surfaces that
-- are in sync-mode.
commitTree :: Int
           -> SObject WlSurface
           -> State SurfacesData [(SurfaceId, SurfaceState (V2 Int32, SurfaceId))]
commitTree n surfObj = do
    msurf <- gets $ fmap (id &&& inSync) . lookupSurface surfObj
    case (msurf, n) of
      (Nothing        , _) -> return [] -- couldn't find the surface - skip
      (Just (_, True ), 0) -> return [] -- top-level surface in sync-mode - skip
      (Just (_, False), 1) -> return [] -- second-level surface not in sync - skip
      (Just (s, _    ), _) -> do
          -- any other surface - commit and check its children
          modify $ insertSurface surfObj (s { sdCached = Nothing })
          (((,) (surfaceToId surfObj) <$> maybeToList (sdCached s)) ++)
            . concat
            <$> mapM (commitTree (n + 1)) (children s)
    where
        children = map snd . uncurry (++) . sdChildren
        inSync s = sdInheritedSync s || sdSync s

-- | Updates the sync flag of a surfaces.
--
-- If the sync flag changes from synchronized to unsynchronized we have to
-- commit any stashed state the surface has. We also have to make sure the new
-- sync state is propagated through to its children.
setSync :: SObject WlSurface -> Bool -> SurfacesData -> ([(SurfaceId, SurfaceState (V2 Int32, SurfaceId))], SurfacesData)
setSync surfaceObj sync sd = fromMaybe ([], sd) $ do
    surf <- lookupSurface surfaceObj sd

    let newSync = sdInheritedSync surf || sync
        oldSync = sdInheritedSync surf || sdSync surf

    return . (`runState` sd) $ do
        when (not (sdInheritedSync surf) && newSync /= oldSync) $
            mapM_ (propagateSync newSync) (children surf)

        if not newSync && oldSync
          then commitTree 0 surfaceObj
          else return []
    where
        children = map snd . uncurry (++) . sdChildren

        propagateSync :: Bool -> SObject WlSurface -> State SurfacesData ()
        propagateSync newSync sobj = do
            msurf <- gets $ lookupSurface sobj
            case msurf of
              Nothing   -> return ()
              Just surf -> do
                  let oldSync = sdInheritedSync surf || sdSync surf
                  modify $ adjustSurface (\s -> s { sdInheritedSync = newSync }) sobj
                  when (newSync /= oldSync) $
                      mapM_ (propagateSync (newSync || sdSync surf)) (children surf)

-- | Commits a surface and any of its children that are not in sync-mode.
--
-- If the surface itself is in sync-mode the state will be cached and applied
-- when the parent is committed.
commit :: SObject WlSurface -> SurfacesData -> ([(SurfaceId, SurfaceState (V2 Int32, SurfaceId))], SurfacesData)
commit surf =
    runState (commitTree 0 surf)
    . adjustSurface (\s -> nextSurfaceData $ s { sdCached = Just (toSurfaceState s) } ) surf

-- | The initial surface state.
initialSurfacesData :: SurfacesData
initialSurfacesData =
    SurfacesData { surfaces  = M.empty
                 , callbacks = M.empty
                 }

nextSurfaceData :: SurfaceData -> SurfaceData
nextSurfaceData sd =
    sd { sdDamageSurface  = R.empty
       , sdDamageBuffer   = R.empty
       , sdBuffer         = Nothing
       , sdFrameCallbacks = []
       }

initialSurfaceData :: SurfaceData
initialSurfaceData =
    SurfaceData { sdDamageSurface   = R.empty
                , sdDamageBuffer    = R.empty
                , sdOpaque          = R.empty
                , sdInput           = R.everything
                , sdBuffer          = Nothing
                , sdBufferOffset    = 0
                , sdBufferTransform = WlOutputTransformNormal
                , sdBufferScale     = 1
                , sdFrameCallbacks  = []
                , sdSubsurface      = Nothing
                , sdChildren        = ([], [])
                , sdSync            = False
                , sdInheritedSync   = False
                , sdCached          = Nothing
                }
