module Woburn.Input
    ( KeyState (..)
    , MouseButton (..)
    , TimeStamp
    , RawInput (..)
    )
where

import Data.Int
import Data.Word
import Linear
import Woburn.Output

-- | The state of a keyboard key or mouse button.
data KeyState =
    Pressed
  | Released
  deriving (Eq, Show, Enum)

-- | A mouse button.
data MouseButton =
    LeftButton
  | MiddleButton
  | RightButton
  | OtherButton Word32
  deriving (Eq, Show)

-- | Timestamp in milliseconds with an unknown base.
type TimeStamp = Word32

-- | Raw input events sent by the backend to the core.
data RawInput =
    MotionAbsolute OutputId (V2 Word32)
  | MotionRelative (V2 Int32)
  | Button MouseButton KeyState
  deriving (Eq, Show)
