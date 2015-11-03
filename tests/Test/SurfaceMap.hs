{-# LANGUAGE TemplateHaskell #-}
module Test.SurfaceMap
    ( surfaceMapTests
    )
where

import Control.Arrow
import Data.Foldable
import Data.Maybe
import Data.List
import Data.Region
import Data.STree
import qualified Data.STree.Zipper as Z
import Linear
import Prelude hiding (foldr)
import Woburn.Protocol
import Woburn.Surface
import qualified Woburn.Surface.Map as SM
import Test.Arbitrary ()
import Test.QuickCheck hiding (label)
import Test.QuickCheck.Monadic

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

checkTree :: SM.SurfaceMap SurfaceId -> STree SurfaceId -> Property
checkTree sm tree = conjoin . map ((=== Just tree) . (`SM.lookupSTree` sm)) $ toList tree

prop_insert :: [SurfaceId] -> Property
prop_insert ids =
    mapM (`SM.lookupSurfaces` surfaceMapFromIds ids) ids === Just (map (singleton . create) ids)

prop_attach :: STree SurfaceId -> Property
prop_attach stree =
    let ids = toList stree
        sm  = surfaceMapFromSTrees [stree]
    in
    forAll (elements ids) $ \sid -> (sm >>= SM.lookupSTree sid) === Just stree

prop_delete :: STree SurfaceId -> Property
prop_delete stree =
    monadic (fromMaybe $ property False) $ do
        sid <- pick . elements $ toList stree
        sm  <- run $ SM.delete sid =<< surfaceMapFromSTrees [stree]

        let trees =
                (\(ps, STree ls _ rs) -> ps ++ ls ++ rs) $
                    maybe
                    ([], stree)
                    (first $ (:[]) . Z.toTree)
                    (Z.delete =<< Z.findFirst ((== sid) . label) stree)

        stop $
            sort (concatMap toList trees) === sort (filter (/= sid) (toList stree))
            .&&. SM.lookupSTree sid sm === Nothing
            .&&. SM.lookupSurfaces sid sm === Nothing
            .&&. conjoin (map (checkTree sm) trees)

prop_detach :: STree SurfaceId -> Property
prop_detach stree =
    monadic (fromMaybe $ property False) $ do
        sid <- pick . elements $ toList stree
        sm  <- run $ SM.attach sid Nothing =<< surfaceMapFromSTrees [stree]

        let trees =
                uncurry (flip (:)) $
                    maybe
                    ([], stree)
                    (first $ (:[]) . Z.toTree)
                    (Z.delete =<< Z.findFirst ((== sid) . label) stree)

        stop $ conjoin (map (checkTree sm) trees)

return []
surfaceMapTests :: IO Bool
surfaceMapTests = $quickCheckAll
