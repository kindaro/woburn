{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Test.MChan
    ( mChanTests
    )
where

import Control.Concurrent.MChan.Split
import Control.Monad.Writer
import Data.Maybe
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- | Check that closeMChan works as expected.
prop_isClosed :: Property
prop_isClosed = monadicIO $ do
    (r,w) <- run newMChan
    run $ closeMChan w

    c <- run $ isClosedMChan w
    x <- run $ readMChan r
    assert c
    assert $ isNothing x

-- | Check that values written after the channel has been closed is discarded.
prop_readWrite :: [Int] -> [Int] -> Property
prop_readWrite as bs = monadicIO $ do
    (r,w) <- run newMChan

    run $ mapM_ (writeMChan w) as
    run $ closeMChan w
    run $ mapM_ (writeMChan w) bs

    rs <- execWriterT $ readUntilClosed r (tell . (:[]))
    assert (rs == as)

return []
mChanTests :: IO Bool
mChanTests = $quickCheckAll
