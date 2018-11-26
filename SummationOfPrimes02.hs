import Data.Numbers.Primes
import Control.Monad

sumPs :: Int -> Int
sumPs m = sum $ takeWhile (<= m) $ primes

stringToInt :: String -> Int
stringToInt = read

main :: IO ()
main = do
  n <- fmap stringToInt getLine
  replicateM_ n (do
                    m <- fmap stringToInt getLine
                    print $ sumPs m)
