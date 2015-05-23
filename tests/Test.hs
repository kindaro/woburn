import Control.Applicative
import Control.Monad
import System.Exit
import Test.SurfaceSet
import Test.Zipper
    
tests :: [IO Bool]
tests = [ surfaceSetTests
        , zipperTests
        ]

main :: IO ()
main = do
    res <- and <$> sequence tests
    unless res exitFailure
