{-# LANGUAGE TemplateHaskell #-}
module Test.SurfaceMap
    ( surfaceMapTests
    )
where

import Control.Arrow
import Control.Monad
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

dummyState :: SurfaceState
dummyState =
    SurfaceState { surfBuffer = Nothing
                 , surfBufferOffset = V2 0 0
                 , surfBufferScale  = 1
                 , surfDamage       = everything
                 , surfOpaque       = everything
                 , surfInput        = everything
                 , surfTransform    = WlOutputTransformNormal
                 }

surfaceMapFromIds :: [SurfaceId] -> SM.SurfaceMap SurfaceId
surfaceMapFromIds = foldl' (\sm sid -> SM.insert sid (create sid) sm) SM.empty

commitSTree :: STree SurfaceId -> SM.SurfaceMap SurfaceId -> Maybe (SM.SurfaceMap SurfaceId)
commitSTree (STree ls sid rs) sm = do
    sml <- foldlM (flip commitSTree) sm  ls
    smr <- foldlM (flip commitSTree) sml rs
    snd <$> SM.setState dummyState sid smr

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

prop_setState :: STree SurfaceId -> Property
prop_setState stree@(STree _ root _) =
    let ids = filter (/= root) $ toList stree
    in
    monadic (fromMaybe $ property False) $ do
        pre (not $ null ids)
        idsWithSync <- pick $ sublistOf ids
        idToCommit  <- pick $ elements ids
        ptr         <- run $ Z.findFirst ((== idToCommit) . label) stree

        let parents = map (label . Z.getTree) $ Z.parents ptr
            inSync  = any (`elem` idsWithSync) $ idToCommit : parents
            firstNonSync = last . map snd . takeWhile (not . fst) . reverse $ map ((`elem` idsWithSync) &&& id) (parents ++ [root])
            ptr' = fmap (addSync idsWithSync . create) ptr
            expectedSurf = addSync idsWithSync . addState $ create idToCommit
            expectedPtr = if inSync
                             then Z.modify (\(STree l _ r) -> STree l expectedSurf r) ptr'
                             else ptr'

        sm1       <- run $ surfaceMapFromSTrees [stree]
        sm2       <- run $ foldlM (\m sid -> snd <$> SM.setSync True sid m) sm1 idsWithSync
        (ss, sm3) <- run $ SM.setState dummyState idToCommit sm2
        sid       <- pick $ elements (root : ids)
        surfTree1 <- run $ SM.lookupSurfaces sid sm3
        (ts, sm4) <- run $ SM.setState dummyState firstNonSync sm3
        surfTree2 <- run $ SM.lookupSurfaces sid sm4

        stop $
            ss === [expectedSurf | not inSync]
            .&&. ts === [addState $ create firstNonSync] ++ [expectedSurf | inSync]
            .&&. surfTree2 === fmap (addSync idsWithSync . create) stree
            .&&. surfTree1 === Z.toTree expectedPtr
    where
        addState surf = surf { surfState = Just dummyState }
        addSync sids surf
            | surfData surf `elem` sids = surf { surfSync = True }
            | otherwise                 = surf

genShuffles :: SurfaceId -> Int -> [SurfaceId] -> Gen ([(ShuffleOperation, SurfaceId, SurfaceId)], [SurfaceId])
genShuffles root n ids
    | n <= 0 || l < 2 = return ([], ids)
    | otherwise = do
        a  <- choose (0, l - 1)
        b  <- choose (0, l - 1) `suchThat` (/= a)
        c  <- choose (0, l - 1) `suchThat` ((/= root) . (ids !!))
        op <- elements $ [PlaceAbove | a > 0    ] ++
                          [PlaceBelow | a < l - 1] ++
                          [DeletedAbove]

        let (splitPoint, del) =
                case op of
                  PlaceAbove   -> (a, b)
                  PlaceBelow   -> (a + 1, b)
                  DeletedAbove -> (a, c)
                  _            -> error "This shouldn't happen"
            eA       = ids !! a
            eB       = ids !! del
            (as, bs) = splitAt splitPoint ids
            ids'     = filter (/= eB) as ++ [eB | op /= DeletedAbove] ++ filter (/= eB) bs

        first ((op, eB, eA) :) <$> genShuffles root (n - 1) ids'
    where
        l = length ids


idsToSTree :: SurfaceId -> [SurfaceId] -> STree SurfaceId
idsToSTree root ids =
    let (ls, n:rs) = span (/= root) ids
    in
    STree (map singleton ls) n (map singleton rs)

prop_shuffle :: Int -> Int -> Property
prop_shuffle i j =
    i > 1 ==>
    let ids@(root:_) = take i [0..]
        stree        = idsToSTree root ids
    in
    forAll (genShuffles root j ids) $ \(shs, expected) ->
    forAll (elements expected) $ \sid ->
        (
            surfaceMapFromSTrees [stree]
            >>= flip (foldlM applyOp) shs
            >>= fmap snd . SM.setState dummyState root
            >>= SM.lookupSTree sid
        ) === Just (idsToSTree root expected)
    where
        applyOp m (op, a, b) =
            case op of
              DeletedAbove -> SM.delete a m
              _            -> SM.addShuffle op a b m

prop_setSync :: STree SurfaceId -> Property
prop_setSync stree@(STree _ root _) =
    monadic (fromMaybe $ property False) $ do
        let ids = toList stree

        idsWithState <- pick $ sublistOf ids
        (ss, sm)     <- run  $
            surfaceMapFromSTrees [stree]
            >>= flip (foldlM (\(s, m) sid -> first (++ s) <$> SM.setSync True sid m)) ids . (,) []

        (surfs, sm') <- run  $
            foldlM (\m sid -> snd <$> SM.setState dummyState sid m) sm idsWithState
            >>= SM.setSync False root

        sid      <- pick $ elements ids
        surfTree <- run  $ SM.lookupSurfaces sid sm'

        let (expectedSurfs, expectedSurfTree) = expected 0 idsWithState stree

        stop
            $    ss         === []
            .&&. sort surfs === sort expectedSurfs
            .&&. surfTree   === expectedSurfTree
    where
        expected :: Int -> [SurfaceId] -> STree SurfaceId -> ([Surface SurfaceId], STree (Surface SurfaceId))
        expected n idsWithState tree@(STree ls sid rs)
            | n == 1 && root `notElem` idsWithState =
                ([], fmap (createSurf True idsWithState) tree)
            | otherwise =
                let (lss', ls') = first concat . unzip $ map (expected (n + 1) idsWithState) ls
                    (rss', rs') = first concat . unzip $ map (expected (n + 1) idsWithState) rs
                    s           = createSurf (sid /= root) idsWithState sid
                in
                if isNothing (surfState s)
                  then (     lss' ++ rss' , STree ls' s rs')
                  else (s : (lss' ++ rss'), STree ls' (committed s) rs')

        createSurf sync idsWithState sid =
            (create sid) { surfSync  = sync
                         , surfState = mfilter (const (sid `elem` idsWithState)) (Just dummyState)
                         }

return []
surfaceMapTests :: IO Bool
surfaceMapTests = $quickCheckAll
