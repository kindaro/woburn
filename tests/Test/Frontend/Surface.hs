{-# LANGUAGE TemplateHaskell #-}
module Test.Frontend.Surface
    ( frontendSurfaceTests
    )
where

import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.Monoid
import Data.Int
import Graphics.Wayland
import Linear
import Woburn.Frontend.Types.Surface
import Woburn.Protocol.Core
import Woburn.Surface
import Test.Arbitrary
import Test.QuickCheck hiding (label, shuffle)

prop_insertSurface :: SObject WlSurface -> Property
prop_insertSurface obj =
    let sd = insertSurface obj initialSurfaceData initialSurfacesData
     in lookupSurface obj sd === Just initialSurfaceData

prop_deleteSurface :: SObject WlSurface -> Property
prop_deleteSurface obj =
    let sd = insertSurface obj initialSurfaceData initialSurfacesData
     in lookupSurface obj sd                     === Just initialSurfaceData .&&.
        lookupSurface obj (deleteSurface obj sd) === Nothing

prop_adjustSurface :: SObject WlSurface -> V2 Int32 -> Property
prop_adjustSurface obj off =
    let f s = s { sdBufferOffset = off }
        sd  = adjustSurface f obj (insertSurface obj initialSurfaceData initialSurfacesData)
     in lookupSurface obj sd === Just (f initialSurfaceData)

prop_callbacks :: SurfaceId -> [SObject WlCallback] -> Property
prop_callbacks sid cbs =
    let sd = insertCallbacks sid cbs initialSurfacesData
     in lookupCallbacks sid sd                       === cbs .&&.
        lookupCallbacks sid (deleteCallbacks sid sd) === []

prop_addSubsurface :: SObject WlSurface
                   -> SObject WlSubsurface
                   -> SObject WlSurface
                   -> Bool
                   -> Bool
                   -> Property
prop_addSubsurface surf subSurf parent parentSync parentInheritedSync =
    let sd = addSubsurface surf subSurf parent
             . insertSurface surf initialSurfaceData
             $ insertSurface parent (initialSurfaceData { sdSync          = parentSync
                                                        , sdInheritedSync = parentInheritedSync
                                                        }
                                    ) initialSurfacesData
        Just pd = lookupSurface parent sd
        Just cd = lookupSurface surf sd

     in
     surf /= parent ==>
     conjoin [ sdSubsurface    cd === Just subSurf
             , sdChildren      pd === ([(0, surf)], [])
             , sdSync          pd === parentSync
             , sdInheritedSync pd === parentInheritedSync
             , sdSync          cd === True
             , sdInheritedSync cd === (parentSync || parentInheritedSync)
             ]

prop_delSubsurface :: SObject WlSurface
                   -> SObject WlSubsurface
                   -> SObject WlSurface
                   -> Bool
                   -> Bool
                   -> Property
prop_delSubsurface surf subSurf parent parentSync parentInheritedSync =
    let sd = delSubsurface surf parent
             . addSubsurface surf subSurf parent
             . insertSurface surf initialSurfaceData
             $ insertSurface parent (initialSurfaceData { sdSync          = parentSync
                                                        , sdInheritedSync = parentInheritedSync
                                                        }
                                    ) initialSurfacesData
        Just pd = lookupSurface parent sd
        Just cd = lookupSurface surf sd
     in
     surf /= parent ==>
     conjoin [ sdSubsurface    cd === Nothing
             , sdChildren      pd === ([], [])
             , sdSync          pd === parentSync
             , sdInheritedSync pd === parentInheritedSync
             , sdSync          cd === False
             , sdInheritedSync cd === False
             ]

prop_setPosition :: SObject WlSurface
                 -> SObject WlSubsurface
                 -> SObject WlSurface
                 -> SObject WlSubsurface
                 -> SObject WlSurface
                 -> V2 Int32
                 -> V2 Int32
                 -> Property
prop_setPosition surfA subSurfA surfB subSurfB parent posA posB =
    let sd = setPosition surfA posA parent
             . setPosition surfB posB parent
             . addSubsurface surfB subSurfB parent
             . addSubsurface surfA subSurfA parent
             . insertSurface surfA initialSurfaceData
             . insertSurface surfB initialSurfaceData
             $ insertSurface parent initialSurfaceData initialSurfacesData

        Just pd = lookupSurface parent sd

     in
     surfA /= parent && surfB /= parent && surfA /= surfB ==>
     sdChildren pd === ([(posB, surfB), (posA, surfA)], [])

genShuffles :: [SObject WlSurface]
            -> Gen [(SObject WlSurface, SObject WlSurface, Bool)]
genShuffles objs = sized $ \n -> vectorOf n genShuffle
    where
        genShuffle =
            ((,,) <$> elements objs <*> elements objs <*> arbitrary)
            `suchThat` (\(a, b, _) -> a /= b)

applyShuffles :: [SObject WlSurface]
              -> [(SObject WlSurface, SObject WlSurface, Bool)]
              -> ([(V2 Int32, SObject WlSurface)], [(V2 Int32, SObject WlSurface)])
applyShuffles objs@(parent:_) =
    (map ((,) 0) *** map ((,) 0))
    . second tail
    . span (/= parent)
    . reverse
    . foldr applyShuffle objs
    where
        applyShuffle (a, b, above) =
            (\(as, _:bs) -> as ++ if above then b:a:bs else a:b:bs)
            . span (/= b)
            . filter (/= a)

prop_shuffle :: Int -> Property
prop_shuffle n =
    n > 1 && n < 1000 ==>
    let objs@(parent:children) = map (Object . fromIntegral) [0..n]
        subSurf = Object 0
        sd      = foldr
                    (\c -> addSubsurface c subSurf parent . insertSurface c initialSurfaceData)
                    (insertSurface parent initialSurfaceData initialSurfacesData)
                    (reverse children)
     in
     forAll (genShuffles objs) $ \shuffles ->
         let sd'     = foldr shuffle sd shuffles
             Just pd = lookupSurface parent sd'
             shuffle (a, b, True ) = placeAbove a b parent
             shuffle (a, b, False) = placeBelow a b parent
          in sdChildren pd === applyShuffles objs shuffles

prop_commitPropagates :: Tree (Int, Bool) -> Property
prop_commitPropagates tree' =
    let tree = (\(Tree l (obj, _) r) -> Tree l (obj, False) r) $ fmap (first (Object . ObjId . fromIntegral)) tree'
        sd   = createFromTree tree initialSurfacesData
        root = fst $ label tree
        objs = filter (/= root) . toList $ fmap fst tree
     in
     objs /= [] ==>
     forAll (elements objs) $ \c ->
         let (dataC, sd') = commit c sd
             (dataR, _  ) = commit root sd'
             depth        = getFirst $ surfSyncDepth (First Nothing) 0 c tree
          in
          counterexample ("depth = " ++ show depth) .
          counterexample ("dataC = " ++ show dataC) .
          counterexample ("dataR = " ++ show dataR) .
          counterexample ("sd    = " ++ show sd) $
              case depth of
                Nothing -> map fst dataC === [surfaceToId c] .&&. map fst dataR === [surfaceToId root]
                Just 1  -> dataC === [] .&&. map fst dataR === [surfaceToId root, surfaceToId c]
                _       -> dataC === [] .&&. map fst dataR === [surfaceToId root]
    where
        subsurf :: SObject WlSubsurface
        subsurf = Object 0

        surfSyncDepth :: First Int -> Int -> SObject WlSurface -> Tree (SObject WlSurface, Bool) -> First Int
        surfSyncDepth inherited depth obj (Tree l n r)
            | obj == fst n = newDepth
            | otherwise    = mconcat (map (surfSyncDepth newDepth (depth + 1) obj) (l ++ r))
            where
                newDepth = inherited <> (First $ guard (snd n) *> pure depth)

        createFromTree :: Tree (SObject WlSurface, Bool) -> SurfacesData -> SurfacesData
        createFromTree (Tree l (sobj, _) r) =
            flip (foldr (\t -> snd . uncurry setSync (label t))) (l ++ r)
            . flip (foldr (\t -> addSubsurface (fst $ label t) subsurf sobj)) (l ++ r)
            . flip (foldr createFromTree) (l ++ r)
            . insertSurface sobj initialSurfaceData

prop_commitCallbacks :: SObject WlSurface -> [SObject WlCallback] -> [SObject WlCallback] -> Property
prop_commitCallbacks surf cba cbb =
    let sid = surfaceToId surf
        sd  = snd . commit surf
              . insertCallbacks sid cba
              . insertCallbacks sid cbb
              $ insertSurface surf initialSurfaceData initialSurfacesData
     in lookupCallbacks sid sd === cba ++ cbb

return []
frontendSurfaceTests :: IO Bool
frontendSurfaceTests = $quickCheckAll
