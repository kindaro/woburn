{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Test.SurfaceSet
    ( surfaceSetTests
    )
where

import Control.Applicative
import Control.Arrow
import Data.Foldable (toList)
import Data.List (find, sort, subsequences)
import Data.Maybe
import Data.Monoid
import Data.STree
import Data.STree.Zipper hiding (delete)
import Woburn.Surface
import Woburn.SurfaceSet
import Test.Arbitrary ()
import Test.QuickCheck hiding (label)

setIds :: SurfaceSet a -> [SurfaceId]
setIds = toList . fmap surfId

getChildren :: SurfaceSet a -> [SurfaceSet a]
getChildren (STree l _ r) = l ++ r

withChildren :: SurfaceSet a -> [SurfaceSet a]
withChildren = map snd . filter fst . map ((not . null . getChildren &&& id) . getTree) . zippers

prop_findSid :: SurfaceSet () -> SurfaceId -> Property
prop_findSid set sid =
    case findSid sid set of
         Nothing  -> property $ sid `notElem` setIds set
         Just ptr -> sid === (surfId . label $ getTree ptr)

prop_inheritsSync :: SurfaceSet () -> Property
prop_inheritsSync set =
    forAll (elements (setIds set)) $ \sid ->
        let expected = f False sid set
        in
        (inheritsSync <$> findSid sid set) === expected
        .&&. isJust expected
    where
        f sync sid (STree l n r)
            | surfId n == sid = Just sync
            | otherwise       = getFirst $ mconcat (map (First . f (sync || surfSync n) sid) (l ++ r))

prop_deleteAny :: SurfaceSet () -> SurfaceId -> Property
prop_deleteAny set sid =
    case delete sid set of
         Nothing              ->
             sid === surfId (label set)
             .||. sid `notElem` setIds set
         Just (set', subtree) ->
             let p = do
                     parent@(STree l _ r) <- getTree <$> (findSid sid set >>= goUp)
                     let pid = surfId $ label parent
                     (map (surfId . label) (l ++ [parent] ++ r), ) <$> findSid pid set'
             in
             case p of
                  Nothing -> counterexample "Could not find parent" False
                  Just (cs, pz) ->
                      property (sid `notElem` setIds set')
                      .&&. sid === surfId (label subtree)
                      .&&. sort (setIds set) === sort (setIds set' ++ setIds subtree)
                      .&&. checkShuffle cs (surfShuffle . label $ getTree pz)
    where
        checkShuffle (c:d:es) sh@[Shuffle DeletedAbove aid bid]
            | c == aid  = d == bid
            | otherwise = checkShuffle (d:es) sh
        checkShuffle (c:d:es) sh@[Shuffle DeletedBelow aid bid]
            | d == aid  = c == bid
            | otherwise = checkShuffle (d:es) sh
        checkShuffle _ _ = False

prop_deleteInSet :: SurfaceSet () -> Property
prop_deleteInSet set = forAll (elements $ setIds set) $ prop_deleteAny set

prop_addShuffle :: SurfaceSet () -> Property
prop_addShuffle set =
    let cs = withChildren set
    in not (null cs) ==>
        forAll (elements cs) $ \parent ->
            forAll (shuffleIds parent) $ \(aid, bid) ->
            forAll (oneof [pure PlaceAbove, pure PlaceBelow]) $ \p ->
                let sh = Shuffle p aid bid
                    pid = surfId $ label parent
                in
                case addShuffle sh set of
                         Nothing   -> aid === bid
                         Just set' ->
                             case findSid pid set' of
                              Nothing -> counterexample "Could not find parent" False
                              Just z  -> surfShuffle (label $ getTree z) === [sh]
    where
        shuffleIds ps =
            let cs  = map (surfId . label) $ getChildren ps
                pid = surfId $ label ps
            in
            oneof [ (pid,) <$> elements cs
                  , (,pid) <$> elements cs
                  , elements [(a, b) | a <- cs, b <- cs]
                  ]

applyShuffle :: Shuffle -> SurfaceSet a -> SurfaceSet a
applyShuffle s@(Shuffle DeletedAbove i _) (STree l n r) = STree (f l) n (f r)
    where
        f = map (applyShuffle s) . filter ((/= i) . surfId . label)

applyShuffle s@(Shuffle op           a b) (STree l n r)
    | a `elem` ids && b `elem` ids = fromShuffleList [] . shuffleList $ l ++ [STree [] n []] ++ r
    | otherwise    = STree (map (applyShuffle s) l) n (map (applyShuffle s) r)
    where
        ids = surfId n : map (surfId . label) (l ++ r)
        fromShuffleList _  []     = error "Should not happen"
        fromShuffleList ls (x:rs)
            | label x == n = STree ls n rs
            | otherwise    = fromShuffleList (ls ++ [x]) rs
        shuffleList xs =
            let ea = fromJust $ find ((== a) . surfId . label) xs
                ys = filter ((/= a) . surfId . label) xs
                (ls, rs) = span ((/= b) . surfId . label) ys
            in
            case op of
                 PlaceAbove -> ls ++ (ea : rs)
                 PlaceBelow -> ls ++ (head rs : ea : tail rs)
                 _          -> error "Should not happen"

genShuffle :: SurfaceSet a -> Gen Shuffle
genShuffle (STree l n r) =
    elements
    $  deleteShuffles
    ++ shuffles PlaceAbove
    ++ shuffles PlaceBelow
    where
        deleteShuffles = map ((\i -> Shuffle DeletedAbove i 0) . surfId . label) (l ++ r)
        shuffles op =
            [ Shuffle op a b
            | let ids = surfId n : map (surfId . label) (l ++ r)
            , a <- ids
            , b <- ids
            , a /= b
            ]

genShuffles :: SurfaceSet a -> Gen (SurfaceSet a, [Shuffle])
genShuffles = sized . f []
    where
        f ss set 0 = return (set, ss)
        f ss set n
            | length (toList set) <= 1 = return (set, ss)
            | otherwise                = do
                s <- genShuffle set
                f (s : ss) (applyShuffle s set) (n - 1)

shrinkShuffles :: SurfaceSet a -> (SurfaceSet a, [Shuffle]) -> [(SurfaceSet a, [Shuffle])]
shrinkShuffles set = map (foldr applyShuffle set &&& id) . init . subsequences . snd

prop_shuffle :: SurfaceSet () -> Property
prop_shuffle set =
    forAllShrink (genShuffles set) (shrinkShuffles set) $ \(expSet, ss) ->
        let actSet = foldr f (Just set) ss >>= shuffle
            os     = drawTree $ fmap (show . surfId) set
            es     = drawTree $ fmap (show . surfId) expSet
            as     = case actSet of
                          Nothing -> "Nothing"
                          Just s  -> drawTree (fmap (show . surfId) s)
        in
        counterexample ("original\n" ++ os ++ "====\n" ++ es ++ "\n/=\n" ++ as) $
            Just expSet == actSet
    where
        f (Shuffle DeletedAbove x _) s = fst <$> (s >>= delete x)
        f sh                         s = s >>= addShuffle sh

return []
surfaceSetTests :: IO Bool
surfaceSetTests = $quickCheckAll
