{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Test.SurfaceTree
    ( surfaceTreeTests
    )
where

import Control.Applicative
import Control.Arrow
import Data.Foldable (toList)
import Data.List (find, sort, subsequences)
import Data.Maybe
import Data.STree
import Data.STree.Zipper hiding (delete)
import Woburn.Surface
import Woburn.Surface.Tree
import Test.Arbitrary ()
import Test.QuickCheck hiding (label)

getChildren :: STree SurfaceId -> [STree SurfaceId]
getChildren (STree l _ r) = l ++ r

withChildren :: STree SurfaceId -> [STree SurfaceId]
withChildren = map snd . filter fst . map ((not . null . getChildren &&& id) . getTree) . zippers

prop_findSid :: STree SurfaceId -> SurfaceId -> Property
prop_findSid set sid =
    case findSid sid set of
         Nothing  -> property $ sid `notElem` toList set
         Just ptr -> sid === label (getTree ptr)

prop_deleteAny :: STree SurfaceId -> SurfaceId -> Property
prop_deleteAny set sid =
    case delete sid set of
         Nothing ->
             sid === label set
             .||. sid `notElem` toList set
         Just (set', subtree, sh) ->
             let p = do
                     parent@(STree l _ r) <- getTree <$> (findSid sid set >>= up)
                     let pid = label parent
                     (map label (l ++ [parent] ++ r), ) <$> findSid pid set'
             in
             case p of
                  Nothing -> counterexample "Could not find parent" False
                  Just (cs, pz) ->
                      property (sid `notElem` toList set')
                      .&&. sid === label subtree
                      .&&. sort (toList set) === sort (toList set' ++ toList subtree)
                      .&&. checkShuffle cs sh
    where
        checkShuffle (c:d:es) sh@(Shuffle DeletedAbove aid bid)
            | c == aid  = d == bid
            | otherwise = checkShuffle (d:es) sh
        checkShuffle (c:d:es) sh@(Shuffle DeletedBelow aid bid)
            | d == aid  = c == bid
            | otherwise = checkShuffle (d:es) sh
        checkShuffle _ _ = False

prop_deleteInSet :: STree SurfaceId -> Property
prop_deleteInSet set = forAll (elements $ toList set) $ prop_deleteAny set

prop_findCommonRoot :: STree SurfaceId -> Property
prop_findCommonRoot set =
    let cs = withChildren set
    in not (null cs) ==>
        forAll (elements cs) $ \parent ->
            forAll (shuffleIds parent) $ \(aid, bid) ->
                case findCommonRoot aid bid set of
                         Nothing  -> aid === bid
                         Just pid -> pid === label parent
    where
        shuffleIds ps =
            let cs  = map label $ getChildren ps
                pid = label ps
            in
            oneof [ (pid,) <$> elements cs
                  , (,pid) <$> elements cs
                  , elements [(a, b) | a <- cs, b <- cs]
                  ]

applyShuffle :: Shuffle -> STree SurfaceId -> STree SurfaceId
applyShuffle s@(Shuffle DeletedAbove i _) (STree l n r) = STree (f l) n (f r)
    where
        f = map (applyShuffle s) . filter ((/= i) . label)

applyShuffle s@(Shuffle op           a b) (STree l n r)
    | a `elem` ids && b `elem` ids = fromShuffleList [] . shuffleList $ l ++ [STree [] n []] ++ r
    | otherwise    = STree (map (applyShuffle s) l) n (map (applyShuffle s) r)
    where
        ids = n : map label (l ++ r)
        fromShuffleList _  []     = error "Should not happen"
        fromShuffleList ls (x:rs)
            | label x == n = STree ls n rs
            | otherwise    = fromShuffleList (ls ++ [x]) rs
        shuffleList xs =
            let ea = fromJust $ find ((== a) . label) xs
                ys = filter ((/= a) . label) xs
                (ls, rs) = span ((/= b) . label) ys
            in
            case op of
                 PlaceAbove -> ls ++ (ea : rs)
                 PlaceBelow -> ls ++ (head rs : ea : tail rs)
                 _          -> error "Should not happen"

genShuffle :: STree SurfaceId -> Gen Shuffle
genShuffle (STree l n r) =
    elements
    $  deleteShuffles
    ++ shuffles PlaceAbove
    ++ shuffles PlaceBelow
    where
        deleteShuffles = map ((\i -> Shuffle DeletedAbove i 0) . label) (l ++ r)
        shuffles op =
            [ Shuffle op a b
            | let ids = n : map label (l ++ r)
            , a <- ids
            , b <- ids
            , a /= b
            ]

genShuffles :: STree SurfaceId -> Gen (STree SurfaceId, [Shuffle])
genShuffles = sized . f []
    where
        f ss set 0 = return (set, ss)
        f ss set n
            | length (toList set) <= 1 = return (set, ss)
            | otherwise                = do
                s <- genShuffle set
                f (s : ss) (applyShuffle s set) (n - 1)

shrinkShuffles :: STree SurfaceId -> (STree SurfaceId, [Shuffle]) -> [(STree SurfaceId, [Shuffle])]
shrinkShuffles set = map (foldr applyShuffle set &&& id) . init . subsequences . snd

prop_shuffle :: STree SurfaceId -> Property
prop_shuffle set =
    forAllShrink (genShuffles set) (shrinkShuffles set) $ \(expSet, ss) ->
        let actSet = foldr f (Just ([], set)) ss >>= uncurry shuffle
            os     = drawTree $ fmap show set
            es     = drawTree $ fmap show expSet
            as     = case actSet of
                          Nothing -> "Nothing"
                          Just s  -> drawTree (fmap show s)
        in
        counterexample ("original\n" ++ os ++ "====\n" ++ es ++ "\n/=\n" ++ as) $
            Just expSet == actSet
    where
        f sh s = do
            (ss, set) <- s
            case sh of
                 (Shuffle DeletedAbove x _) -> delete x set >>= \(set', _, sh') -> return (sh' : ss, set')
                 _                          -> return (sh : ss, set)

return []
surfaceTreeTests :: IO Bool
surfaceTreeTests = $quickCheckAll
