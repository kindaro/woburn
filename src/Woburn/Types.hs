{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Woburn.Types
    ( ClientId
    )
where

import Data.Word

newtype ClientId = ClientId Word32
    deriving (Eq, Ord, Show, Num, Real, Integral, Enum, Bounded)

