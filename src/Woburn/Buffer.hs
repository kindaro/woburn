module Woburn.Buffer
    ( Buffer (..)
    , touchBuffer
    )
where

import Data.Word
import Foreign.ForeignPtr
import Woburn.Protocol

data Buffer =
    Buffer { bufData   :: ForeignPtr Word8  -- ^ A pointer to the start of the buffer data.
           , bufFormat :: WlShmFormat       -- ^ The buffer format.
           , bufWidth  :: Word              -- ^ Width in pixels.
           , bufHeight :: Word              -- ^ Height in pixels.
           , bufStride :: Word              -- ^ Row-stride in bytes.
           }
    deriving (Eq, Show)

-- | Ensure that the 'ForeignPtr' pointing to the buffer data is alive before this call.
touchBuffer :: Buffer -> IO ()
touchBuffer = touchForeignPtr . bufData
