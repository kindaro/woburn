{-|
Module      : Control.Concurrent.MChan
Description : A channel that can be closed.
Copyright   : (C) Sivert Berg, 2015
License     : GPL3
Maintainer  : code@trev.is
Stability   : Experimental

A channel that can be closed. Implemented by simply lifting
Control.Concurrent.STM.TMChan from STM to IO.
|-}
module Control.Concurrent.MChan
    ( MChan
    , newMChan
    , readMChan
    , writeMChan
    , closeMChan
    , isClosedMChan
    )
where

import Control.Concurrent.STM
import Control.Concurrent.STM.TMChan

newtype MChan a = MChan { unMChan :: TMChan a }

-- | Creates a new 'MChan'.
newMChan :: IO (MChan a)
newMChan = MChan <$> atomically newTMChan

-- | Reads a value from the channel, returning 'Nothing' if it has been closed.
readMChan :: MChan a -> IO (Maybe a)
readMChan = atomically . readTMChan . unMChan

-- | Writes a value to the channel.
--
-- If the channel has been closed, the value is silently discarded.
writeMChan :: MChan a -> a -> IO ()
writeMChan m = atomically . writeTMChan (unMChan m)

-- | Closes a channel.
closeMChan :: MChan a -> IO ()
closeMChan = atomically . closeTMChan . unMChan

-- | Checks if the channel has been closed.
isClosedMChan :: MChan a -> IO Bool
isClosedMChan = atomically . isClosedTMChan . unMChan
