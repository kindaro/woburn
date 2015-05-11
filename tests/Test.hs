import Control.Applicative
import Control.Monad
import System.Exit
import Test.Zipper
    
tests :: [IO Bool]
tests = [ zipperTests
        ]

main :: IO ()
main = do
    res <- and <$> sequence tests
    unless res exitFailure
