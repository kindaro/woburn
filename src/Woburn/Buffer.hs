module Woburn.Buffer
    ( Buffer (..)
    , touchBuffer
    , withBuffer
    )
where

import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Woburn.Protocol
import Woburn.Types

data Buffer =
    Buffer { bufData     :: ForeignPtr Word8  -- ^ A pointer to the start of the buffer data.
           , bufFormat   :: WlShmFormat       -- ^ The buffer format.
           , bufWidth    :: Word              -- ^ Width in pixels.
           , bufHeight   :: Word              -- ^ Height in pixels.
           , bufStride   :: Word              -- ^ Row-stride in bytes.
           , bufOffset   :: Int               -- ^ The offset into 'bufData' in bytes.
           , bufClientId :: ClientId          -- ^ The ID of the client this 'Buffer' belongs to.
           }
    deriving (Eq, Ord, Show)

-- | Ensure that the 'ForeignPtr' pointing to the buffer data is alive before this call.
touchBuffer :: Buffer -> IO ()
touchBuffer = touchForeignPtr . bufData

withBuffer :: Buffer -> (Ptr a -> IO b) -> IO b
withBuffer buf f = withForeignPtr (bufData buf) (f . (`plusPtr` bufOffset buf) . castPtr)
