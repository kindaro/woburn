{-# LANGUAGE TupleSections #-}
module Data.STree.Zipper
    ( Zipper
    , ZipperPosition (..)
    -- * Movement
    , up
    , down
    , left
    , right
    , children
    , parents
    -- * Zipper information
    , depth
    , position
    -- * Insertion / deletion
    , delete
    , insert
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
import Data.Foldable
import Data.List (unfoldr)
import Data.Maybe (listToMaybe)
import Data.Monoid
import Data.Sequence (singleton, viewl, ViewL (..), (><), fromList)
import Data.STree hiding (singleton)
import Data.Traversable
import Prelude hiding (Left, Right)

data Direction = Right | Left
    deriving (Eq, Show)

data Branch a = Branch Direction  [STree a] [STree a] a [STree a]
    deriving (Show, Eq)

instance Functor Branch where
    fmap f (Branch dir bs as a xs) =
        Branch dir
        (map (fmap f) bs)
        (map (fmap f) as)
        (f a)
        (map (fmap f) xs)

instance Foldable Branch where
    foldMap f (Branch _ bs as a xs) =
        mconcat [ foldMap (foldMap f) bs
                , foldMap (foldMap f) as
                , f a
                , foldMap (foldMap f) xs
                ]

instance Traversable Branch where
    traverse f (Branch dir bs as a xs) =
        Branch dir
        <$> traverse (traverse f) bs
        <*> traverse (traverse f) as
        <*> f a
        <*> traverse (traverse f) xs

data ZipperPosition =
    Root
  | OnLeft
  | OnRight
  deriving (Show, Eq)

data Zipper a = Zipper (STree a) [Branch a]
    deriving (Show, Eq)

instance Functor Zipper where
    fmap f (Zipper tree bs) = Zipper (fmap f tree) (map (fmap f) bs)

instance Foldable Zipper where
    foldMap f (Zipper tree bs) = foldMap f tree <> foldMap (foldMap f) bs

instance Traversable Zipper where
    traverse f (Zipper tree bs) = Zipper <$> traverse f tree <*> traverse (traverse f) bs

infixr 5 <++>

-- | Concatenates two lists, where the first part is reversed.
(<++>) :: [a] -> [a] -> [a]
a <++> b = reverse a ++ b

-- | Moves to the parent node if one exists, otherwise it returns 'Nothing'.
up :: Zipper a -> Maybe (Zipper a)
up (Zipper _ []                                   ) = Nothing
up (Zipper n (Branch Left  before after x rs : bs)) = Just $ Zipper (STree (before <++> n : after) x rs                     ) bs
up (Zipper n (Branch Right before after x ls : bs)) = Just $ Zipper (STree (reverse ls           ) x (before <++> n : after)) bs

-- | Returns a zipper for the left-most child.
down :: Zipper a -> Maybe (Zipper a)
down (Zipper (STree l x r) bs) =
    case (l, r) of
         ([]  , []  ) -> Nothing
         (c:cs, _   ) -> Just . Zipper c $ Branch Left  [] cs x r : bs
         ([]  , c:cs) -> Just . Zipper c $ Branch Right [] cs x l : bs

-- | Go to the left sibling if one exists.
left :: Zipper a -> Maybe (Zipper a)
left (Zipper _ []    ) = Nothing
left (Zipper n (b:bs)) =
    case b of
         Branch Right []     sr x (l:ls) -> Just $ Zipper l (Branch Left  ls []     x (n:sr) : bs)
         Branch Right (l:sl) sr x ls     -> Just $ Zipper l (Branch Right sl (n:sr) x ls     : bs)
         Branch Left  (l:sl) sr x rs     -> Just $ Zipper l (Branch Left  sl (n:sr) x rs     : bs)
         _                               -> Nothing

-- | Go to the right sibling if one exists.
right :: Zipper a -> Maybe (Zipper a)
right (Zipper _ []    ) = Nothing
right (Zipper n (b:bs)) =
    case b of
         Branch Left  sl []     x (r:rs) -> Just $ Zipper r (Branch Right []     rs x (n:sl): bs)
         Branch Left  sl (r:sr) x rs     -> Just $ Zipper r (Branch Left  (n:sl) sr x rs    : bs)
         Branch Right sl (r:sr) x ls     -> Just $ Zipper r (Branch Right (n:sl) sr x ls    : bs)
         _                               -> Nothing

-- | Returns the number of ancestors this node has.
depth :: Zipper a -> Int
depth = length . parents

-- | Returns the zipper position relative to its parent.
position :: Zipper a -> ZipperPosition
position (Zipper _ bs) =
    case bs of
         []                   -> Root
         Branch dir _ _ _ _:_ ->
             case dir of
               Left  -> OnLeft
               Right -> OnRight

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
toTree z = maybe (getTree z) toTree $ up z

-- | Returns a zipper with the focus on the root-node.
fromTree :: STree a -> Zipper a
fromTree n = Zipper n []

-- | Returns a list of all immediate children of this zipper.
children :: Zipper a -> [Zipper a]
children z =
    case down z of
         Nothing -> []
         Just l  -> l : unfoldr (fmap (id &&& id) . right) l

-- | Returns a list of zippers for all parents of this zipper.
parents :: Zipper a -> [Zipper a]
parents = unfoldr (fmap (id &&& id) . up)

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

-- | Deletes the focused tree, returning a pointer to its parent along with the
-- removed tree.
--
-- If there's no parent, 'Nothing' is returned.
delete :: Zipper a -> Maybe (Zipper a, STree a)
delete (Zipper _ []    ) = Nothing
delete (Zipper n (b:bs)) =
    Just . (, n) $ case b of
                Branch Left  ll lr x r -> Zipper (STree (ll <++> lr) x r           ) bs
                Branch Right rl rr x l -> Zipper (STree (reverse l ) x (rl <++> rr)) bs

-- | Inserts a tree as the left-most child of the focused tree.
insert :: STree a -> Zipper a -> Zipper a
insert a (Zipper (STree l n r) bs) = Zipper (STree (a : l) n r) bs
