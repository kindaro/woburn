module Data.List.Zipper
    ( Zipper
    , empty
    , left
    , right
    , fromList
    , toList
    , cursor
    , insert
    )
where

-- | A list zipper.
data Zipper a = Zipper [a] [a]

empty :: Zipper a
empty = Zipper [] []

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
