import Control.Applicative
import Control.Monad
import Prelude
import System.Exit
import Test.MChan
import Test.Rect
import Test.Region
import Test.STree
import Test.SurfaceMap
import Test.Zipper
import Test.Frontend.Surface

tests :: [IO Bool]
tests = [ surfaceMapTests
        , sTreeTests
        , zipperTests
        , mChanTests
        , regionTests
        , rectTests
        , frontendSurfaceTests
        ]

main :: IO ()
main = do
    res <- and <$> sequence tests
    unless res exitFailure
