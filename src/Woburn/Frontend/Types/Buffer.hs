module Woburn.Frontend.Types.Buffer
    ( BuffersData (..)
    , initialBuffersData
    , insert
    , delete
    , acquire
    , release
    )
where

import Control.Arrow
import qualified Data.Map as M
import Graphics.Wayland hiding (insert, delete)
import Woburn.Buffer
import Woburn.Protocol.Core

data BuffersData =
    BuffersData { buffers   :: M.Map (SObject WlBuffer) Buffer
                , refCounts :: M.Map Buffer (Int, SObject WlBuffer)
                }

initialBuffersData :: BuffersData
initialBuffersData =
    BuffersData { buffers   = M.empty
                , refCounts = M.empty
                }

-- | Insert a new object/buffer mapping.
insert :: SObject WlBuffer -> Buffer -> BuffersData -> BuffersData
insert obj buf bd = bd { buffers = M.insert obj buf (buffers bd) }

-- | Deletes a buffer object, but it does not clear any outstanding references
-- to its underlying buffer.
delete :: SObject WlBuffer -> BuffersData -> BuffersData
delete obj bd = bd { buffers = M.delete obj (buffers bd) }

-- | Finds an object's buffer, and increases its reference count by one.
--
-- If the object does not exist, 'Nothing' is returned.
acquire :: SObject WlBuffer -> BuffersData -> (Maybe Buffer, BuffersData)
acquire obj bd =
    case M.lookup obj (buffers bd) of
      Nothing  -> (Nothing, bd)
      Just buf -> (Just buf, bd { refCounts = M.alter alterEntry buf (refCounts bd) })
    where
        alterEntry Nothing       = Just (1, obj)
        alterEntry (Just (n, o)) = Just (n + 1, o)

-- | Decrements a buffer's reference count by one, returning the new reference
-- count, the buffer and the modified state.
release :: Buffer -> BuffersData -> (Maybe (Int, SObject WlBuffer), BuffersData)
release buf bd =
    second (\m -> bd { refCounts = m })
    $ M.updateLookupWithKey updateEntry buf (refCounts bd)
    where
        updateEntry _ (n, o)
            | n == 1    = Nothing
            | n <  1    = error "Ref count is less than 1, this should never happen"
            | otherwise = Just (n - 1, o)
