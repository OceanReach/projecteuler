import Data.Char (digitToInt)
import Data.List (foldl')
import Control.Monad (replicateM_)


departAndsum :: Integer -> Int
departAndsum n = foldl' (flip ((+) . digitToInt)) 0$ show n

main :: IO ()
main = do
  n <- fmap (read ::String -> Int) getLine
  replicateM_ n (do
                    m <- fmap (read::String-> Int) getLine
                    print $ departAndsum $ 2^m)
