{-|
Module      : Control.Concurrent.MChan.Split
Description : Splits the 'MChan' type into a read and one write part.
Copyright   : (C) Sivert Berg, 2015
License     : GPL3
Maintainer  : code@trev.is
Stability   : Experimental

Contains two types, 'RMChan' and 'WMChan', that separates a 'CMhan' into two
ends that can either be only written or only read. This makes it possible to
pass a channel to a function, and to be sure it will only read from it, or
only write to it.
|-}
module Control.Concurrent.MChan.Split
    ( RMChan
    , WMChan
    , newMChan
    , readMChan
    , writeMChan
    , closeMChan
    , isClosedMChan
    , readUntilClosed
    )
where

import Control.Applicative
import Control.Arrow
import Control.Monad.IO.Class
import qualified Control.Concurrent.MChan as M
import Prelude

-- | An opaque type representing the read end of the channel.
newtype RMChan a = RMChan { unRMChan :: M.MChan a }
-- | An opaque type representing the write end of the channel.
newtype WMChan a = WMChan { unWMChan :: M.MChan a }

-- | Create a new channel, returning the read and write end.
newMChan :: IO (RMChan a, WMChan a)
newMChan = (RMChan &&& WMChan) <$> M.newMChan

-- | Reads a value from the channel, returning 'Nothing' if it has been closed.
readMChan :: RMChan a -> IO (Maybe a)
readMChan = M.readMChan . unRMChan

-- | Writes a value to the channel.
--
-- If the channel has been close, the value will be silently discarded.
writeMChan :: WMChan a -> a -> IO ()
writeMChan = M.writeMChan . unWMChan

-- | Closes a channel.
closeMChan :: WMChan a -> IO ()
closeMChan = M.closeMChan . unWMChan

-- | Checks if a 'WMChan' is closed.
isClosedMChan :: WMChan a -> IO Bool
isClosedMChan = M.isClosedMChan . unWMChan

-- | Reads values from an 'RMChan' and feeds them to a computation until the
-- channel is closed.
readUntilClosed :: MonadIO m => RMChan a -> (a -> m ()) -> m ()
readUntilClosed c m = liftIO (readMChan c) >>= maybe (return ()) (\a -> m a >> readUntilClosed c m)
