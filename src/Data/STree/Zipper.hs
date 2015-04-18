module Data.STree.Zipper
    ( Zipper
    -- * Movement
    , goUp
    , goDown
    , goLeft
    , goRight
    , children
    , parents
    -- * Zipper information
    , depth
    -- * Modify the tree under the cursr
    , getTree
    , setTree
    , modify
    , modifyA
    -- * Converting to and from zippers
    , toTree
    , fromTree
    , zippers
    -- * Searching a 'STree'
    , findAll
    , findFirst
    )
where

import Control.Applicative
import Control.Arrow ((&&&))
import Data.List (unfoldr)
import Data.Maybe (listToMaybe)
import Data.Sequence (singleton, viewl, ViewL (..), (><), fromList)
import Data.STree

data Branch a =
    TurnLeft  [STree a] [STree a] a [STree a]
  | TurnRight [STree a] [STree a] a [STree a]
  deriving (Eq)

data Zipper a = Zipper (STree a) [Branch a]
    deriving (Eq)

-- | Moves to the parent node if one exists, otherwise it returns 'Nothing'.
goUp :: Zipper a -> Maybe (Zipper a)
goUp (Zipper _ []                                   ) = Nothing
goUp (Zipper n (TurnLeft  before after x right : bs)) = Just $ Zipper (STree (reverse before ++ [n] ++ after) x right) bs
goUp (Zipper n (TurnRight before after x left  : bs)) = Just $ Zipper (STree left x  (reverse before ++ [n] ++ after)) bs

-- | Returns a zipper for the left-most child.
goDown :: Zipper a -> Maybe (Zipper a)
goDown (Zipper (STree l x r) bs) =
    case (l, r) of
         ([]  , []  ) -> Nothing
         (c:cs, _   ) -> Just . Zipper c $ TurnLeft  [] cs x r : bs
         ([]  , c:cs) -> Just . Zipper c $ TurnRight [] cs x l : bs

-- | Go to the left sibling if one exists.
goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Zipper _ []    ) = Nothing
goLeft (Zipper n (b:bs)) =
    case b of
         TurnRight []     sr x (l:ls) -> Just $ Zipper l (TurnLeft  (reverse ls) []     x (n:sr) : bs)
         TurnRight (l:sl) sr x ls     -> Just $ Zipper l (TurnRight sl           (n:sr) x ls     : bs)
         TurnLeft  (l:sl) sr x rs     -> Just $ Zipper l (TurnLeft  sl           (n:sr) x rs     : bs)
         _                            -> Nothing

-- | Go to the right sibling if one exists.
goRight :: Zipper a -> Maybe (Zipper a)
goRight (Zipper _ []    ) = Nothing
goRight (Zipper n (b:bs)) =
    case b of
         TurnLeft  sl []     x (r:rs) -> Just $ Zipper r (TurnRight []     rs x (reverse (n:sl)): bs)
         TurnLeft  sl (r:sr) x rs     -> Just $ Zipper r (TurnLeft  (n:sl) sr x rs              : bs)
         TurnRight sl (r:sr) x ls     -> Just $ Zipper r (TurnRight (n:sl) sr x ls              : bs)
         _                            -> Nothing

-- | Returns the number of ancestors this node has.
depth :: Zipper a -> Int
depth = length . parents

-- | Returns the currently focused tree.
getTree :: Zipper a -> STree a
getTree (Zipper n _) = n

-- | Replaces the current tree.
setTree :: STree a -> Zipper a -> Zipper a
setTree n (Zipper _ z) = Zipper n z

modify :: (STree a -> STree a) -> Zipper a -> Zipper a
modify f z = setTree (f $ getTree z) z

modifyA :: Applicative m => (STree a -> m (STree a)) -> Zipper a -> m (Zipper a)
modifyA f z = flip setTree z <$> f (getTree z)

-- | Converts the zipper back to a tree.
toTree :: Zipper a -> STree a
toTree z = maybe (getTree z) toTree $ goUp z

-- | Returns a zipper with the focus on the root-node.
fromTree :: STree a -> Zipper a
fromTree n = Zipper n []

-- | Returns a list of zippers for all children of this zipper.
children :: Zipper a -> [Zipper a]
children (Zipper (STree l x r) bs) = f TurnLeft [] l r ++ f TurnRight [] r l
    where
        f cons before after other =
            case after of
                 []     -> []
                 (a:as) -> Zipper a (cons before as x other : bs) : f cons (a:before) as other

-- | Returns a list of zippers for all parents of this zipper.
parents :: Zipper a -> [Zipper a]
parents = unfoldr (fmap (id &&& id) . goUp)

-- | Returns a list of all zippers for the tree, sorted by top- left- most
-- first.
zippers :: STree a -> [Zipper a]
zippers = f . singleton . fromTree
    where
        f zs =
            case viewl zs of
                 EmptyL    -> []
                 z :< rest -> z : f (rest >< fromList (children z))

-- | Returns zippers for all nodes fulfilling a predicate.
--
-- The found nodes are sorted by top- left-most first.
findAll :: (STree a -> Bool) -> STree a -> [Zipper a]
findAll p = filter (p . getTree) . zippers

-- | Returns the zipper of the top and left-most node fulfilling a predicate,
-- or 'Nothing' if no nodes match.
findFirst :: (STree a -> Bool) -> STree a -> Maybe (Zipper a)
findFirst p = listToMaybe . findAll p
