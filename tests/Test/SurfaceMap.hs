{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.SurfaceMap
    ( surfaceMapTests
    )
where

import Control.Arrow
import Control.Monad
import Control.Monad.State
import Data.Foldable
import Data.Function
import Data.Int
import Data.Ord
import Data.Maybe
import Data.List
import Data.Tuple
import Data.Word
import Linear
import Prelude hiding (foldr)
import Woburn.Surface
import qualified Woburn.Surface.Map as SM
import Test.Arbitrary
import Test.QuickCheck hiding (label)

maxId :: Tree (SurfaceId, (V2 Int32, Surface Int ())) -> SurfaceId
maxId (Tree l (sid, _) r) = maximum (map maxId l ++ [sid] ++ map maxId r)

treeToMap :: Tree (SurfaceId, (V2 Int32, Surface Int ())) -> Gen (SM.SurfaceMap Int)
treeToMap tree = helper SM.empty tree
    where
        firstUnused :: Word32
        firstUnused = fromIntegral $ maxId tree + 1

        unusedSid = fromIntegral <$> choose (firstUnused, maxBound)

        helper :: SM.SurfaceMap Int -> Tree (SurfaceId, (V2 Int32, Surface Int ())) -> Gen (SM.SurfaceMap Int)
        helper sm (Tree l (sid, (_, surf)) r) = do
            extraL <- map ((,) 0) <$> listOf unusedSid
            extraR <- map ((,) 0) <$> listOf unusedSid
            let cl    = map (swap . second fst . label) l
                cr    = map (swap . second fst . label) r
                surf' = modifyState (\s -> s { surfChildren = (cl ++ extraL, cr ++ extraR) }) surf
            foldrM (flip helper) (SM.insert sid surf' sm) (l ++ r)

treeElems :: V2 Int32 -> Tree (SurfaceId, (V2 Int32, Surface Int ())) -> [(V2 Int32, Surface Int ())]
treeElems globalOff (Tree l (_, (off, surf)) r) =
    concatMap (treeElems (globalOff + off)) l
    ++ [(globalOff + off, surf)]
    ++ concatMap (treeElems (globalOff + off)) r

treeIds :: Tree (SurfaceId, (V2 Int32, Surface Int ())) -> [SurfaceId]
treeIds (Tree l (sid, _) r) =
    concatMap treeIds l
    ++ [sid]
    ++ concatMap treeIds r

prop_insert :: SurfaceId -> Surface Int (V2 Int32, SurfaceId) -> Property
prop_insert sid surf =
    let sm = SM.insert sid surf SM.empty
     in SM.lookup sid sm === Just surf

prop_delete :: SurfaceId -> Surface Int (V2 Int32, SurfaceId) -> Property
prop_delete sid surf =
    let sm = SM.delete sid $ SM.insert sid surf SM.empty
     in SM.lookup sid sm === Nothing

prop_elems :: [(SurfaceId, Surface Int (V2 Int32, SurfaceId))] -> Property
prop_elems es' =
    let es = sortBy (comparing fst) $ nubBy ((==) `on` fst) es'
        sm = foldr (uncurry SM.insert) SM.empty es
     in map snd es === SM.elems sm

prop_adjust :: SurfaceId -> Surface Int (V2 Int32, SurfaceId) -> Property
prop_adjust sid surf =
    let f s = s { surfData = 1 + surfData s }
        sm  = SM.adjust f sid $ SM.insert sid surf SM.empty
     in SM.lookup sid sm === Just (f surf)

prop_lookupAll :: Tree (SurfaceId, (V2 Int32, Surface Int ())) -> Property
prop_lookupAll tree@(Tree _ (root, (off, _)) _) =
    forAll (treeToMap tree) $ \sm ->
        SM.lookupAll off root sm === treeElems 0 tree

prop_lookupAllIds :: Tree (SurfaceId, (V2 Int32, Surface Int ())) -> Property
prop_lookupAllIds tree@(Tree _ (root, _) _) =
    forAll (treeToMap tree) $ \sm ->
        SM.lookupAllIds root sm === treeIds tree

return []
surfaceMapTests :: IO Bool
surfaceMapTests = $quickCheckAll
