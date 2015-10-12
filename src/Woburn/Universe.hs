module Woburn.Universe
where

import Data.Rect
import Data.Word
import qualified Data.List.Zipper as Z
import qualified Data.Map as M
import Woburn.Output

data Universe a =
    Universe { screens  :: Z.Zipper (Screen a)
             , hidden   :: [ Workspace a ]
             , floating :: M.Map a (Rect Word32)
             }

data Screen a =
    Screen { workspace :: Workspace a
           , output    :: MappedOutput
           }

data Workspace a =
    Workspace { tag     :: String
              , windows :: Z.Zipper a
              }

-- | Creates a new 'Universe' given a list of workspace tags.
create :: [String] -> Universe a
create ws =
    Universe { screens  = Z.empty
             , hidden   = map (`Workspace` Z.empty) ws
             , floating = M.empty
             }

-- | Sets the outputs.
--
-- Creates a screen for each of the outputs, and hands it a workspace. If there
-- are fewer workspaces than outputs, there will only be created as many
-- workspaces as there are screens.
--
-- Existing screens are removed.
setOutputs :: [MappedOutput] -> Universe a -> Universe a
setOutputs os u =
    Universe { screens  = Z.fromList $ zipWith Screen ws os
             , hidden   = drop (length os) ws
             , floating = floating u
             }
    where
        ws = map workspace (Z.toList $ screens u) ++ hidden u

view :: String -> Universe a -> Universe a
view t u = undefined

greedyView :: String -> Universe a -> Universe a
greedyView t u = undefined

{-
focusDown :: Universe a -> Universe a
focusUp :: Universe a -> Universe a
focusMaster :: Universe a -> Universe a
focusWindow :: a -> Universe a -> Universe a

shift :: String -> Universe a -> Universe a
shiftWin :: String -> a -> Universe a -> Universe a
shiftMaster :: Universe a -> Universe a
swapMaster :: Universe a -> Universe a

insert :: (Ord a, Eq a) => a -> Universe a -> Universe a
delete :: (Ord a, Eq a) => a -> Universe a -> Universe a

float :: a -> Universe a -> Universe a
sink :: a -> Universe a -> Universe a
-}
