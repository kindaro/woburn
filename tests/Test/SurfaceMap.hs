{-# LANGUAGE TemplateHaskell #-}
module Test.SurfaceMap
    ( surfaceMapTests
    )
where

import Data.Foldable
import Data.Maybe
import Data.List
import Data.Region
import Data.STree
import Linear
import Prelude hiding (foldr)
import Woburn.Protocol
import Woburn.Surface
import qualified Woburn.Surface.Map as SM
import Test.Arbitrary ()
import Test.QuickCheck hiding (label)

surfaceMapFromIds :: [SurfaceId] -> SM.SurfaceMap SurfaceId
surfaceMapFromIds = foldl' (\sm sid -> SM.insert sid (create sid) sm) SM.empty

commitSTree :: STree SurfaceId -> SM.SurfaceMap SurfaceId -> Maybe (SM.SurfaceMap SurfaceId)
commitSTree (STree ls sid rs) sm = do
    sml <- foldlM (flip commitSTree) sm  ls
    smr <- foldlM (flip commitSTree) sml rs
    snd <$> SM.setState sid dummyState smr
    where
        dummyState =
            SurfaceState { surfBuffer = Nothing
                         , surfBufferOffset = V2 0 0
                         , surfBufferScale  = 1
                         , surfDamage       = everything
                         , surfOpaque       = everything
                         , surfInput        = everything
                         , surfTransform    = WlOutputTransformNormal
                         }

insertSTree :: STree SurfaceId -> SM.SurfaceMap SurfaceId -> Maybe (SM.SurfaceMap SurfaceId)
insertSTree stree sm =
    helper stree (foldl' (\m sid -> SM.insert sid (create sid) m) sm (toList stree))
    >>= commitSTree stree
    where
        helper :: STree SurfaceId -> SM.SurfaceMap SurfaceId -> Maybe (SM.SurfaceMap SurfaceId)
        helper (STree ls a rs) m =
            foldrM helper m (ls ++ rs)
            >>= flip (foldrM $ \b -> SM.attach (label b) (Just a)) (ls ++ rs)
            >>= addShuffle a rs

        addShuffle _ []    = return
        addShuffle a (r:_) = SM.addShuffle PlaceAbove a (label r)

surfaceMapFromSTrees :: [STree SurfaceId] -> Maybe (SM.SurfaceMap SurfaceId)
surfaceMapFromSTrees = foldlM (flip insertSTree) SM.empty

prop_insert :: [SurfaceId] -> Property
prop_insert ids =
    mapM (`SM.lookupSurfaces` surfaceMapFromIds ids) ids === Just (map (singleton . create) ids)

prop_attach :: STree SurfaceId -> Property
prop_attach stree =
    let ids = foldr (:) [] stree
        sm  = surfaceMapFromSTrees [stree]
    in
    forAll (elements ids) $ \sid -> (sm >>= SM.lookupSTree sid) === Just stree

return []
surfaceMapTests :: IO Bool
surfaceMapTests = $quickCheckAll
