module Data.List.Zipper
    ( Zipper
    , empty
    , isEmpty
    , left
    , right
    , fromList
    , toList
    , cursor
    , insert
    , delete
    , modify
    )
where

-- | A list zipper.
data Zipper a = Zipper [a] [a]

instance Functor Zipper where
    fmap f (Zipper ls rs) = Zipper (map f ls) (map f rs)

-- | Returns the zipper of an empty list.
empty :: Zipper a
empty = Zipper [] []

-- | Checks if the list is empty.
isEmpty :: Zipper a -> Bool
isEmpty (Zipper _ []) = True
isEmpty _             = False

-- | Moves left in the list if possible.
left :: Zipper a -> Maybe (Zipper a)
left (Zipper []     _ ) = Nothing
left (Zipper (l:ls) rs) = Just $ Zipper ls (l:rs)

-- | Moves right in the list if possible.
right :: Zipper a -> Maybe (Zipper a)
right (Zipper _  []    ) = Nothing
right (Zipper _  [_]   ) = Nothing
right (Zipper ls (r:rs)) = Just $ Zipper (r:ls) rs

-- | Creates a zipper from a list.
fromList :: [a] -> Zipper a
fromList = Zipper []

-- | Gets the underlying list.
toList :: Zipper a -> [a]
toList (Zipper ls rs) = reverse ls ++ rs

-- | Returns the current item under the cursor.
cursor :: Zipper a -> Maybe a
cursor (Zipper _ []   ) = Nothing
cursor (Zipper _ (r:_)) = Just r

-- | Inserts an element at the cursor.
insert :: a -> Zipper a -> Zipper a
insert x (Zipper ls rs) = Zipper ls (x:rs)

-- | Deletes an item from the zipper.
delete :: Eq a => a -> Zipper a -> Zipper a
delete x (Zipper ls rs) =
    let ls' = filter (/= x) ls
        rs' = filter (/= x) rs
    in
    case (ls', rs') of
      ([]  , _ ) -> Zipper ls' rs'
      (r:xs, []) -> Zipper xs  [r]
      _          -> Zipper ls' rs'

-- | Modifies the item under the cursor, or does nothing if the list is empty.
modify :: (a -> a) -> Zipper a -> Zipper a
modify f z@(Zipper ls rs) =
    case rs of
      []      -> z
      (r:rrs) -> Zipper ls (f r : rrs)
