import Control.Applicative
import Control.Monad
import System.Exit
import Test.MChan
import Test.SurfaceTree
import Test.Zipper

tests :: [IO Bool]
tests = [ surfaceTreeTests
        , zipperTests
        , mChanTests
        ]

main :: IO ()
main = do
    res <- and <$> sequence tests
    unless res exitFailure
