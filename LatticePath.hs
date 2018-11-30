import Control.Monad(forM, replicateM_, replicateM)
import Data.List (foldl')
import Data.Vector (Vector, generate, (!))


latticePath :: Int -> Int
latticePath g = throghGraph g g


throghGraph :: Int -> Int -> Int
throghGraph gx gy = go 0 0
  where
    go x y
      | x /= gx && y /= gy = right + down
      | x /= gx = right
      | y /= gy = down
      | otherwise = 1
        where
          right = go (x+1) y
          down = go x (y+1)



latticePath' g = throghGraph' g g


throghGraph' gx gy = v ! gx ! gy
  where
    m = gx+1
    n = gy+1
    v = generate m (\x -> generate n (\y -> f x y))

    f 0 0 = 1
    f 0 _ = 1
    f _ 0 = 1
    f i j = v ! (i-1) ! j + v ! i ! (j-1)

latticePath'' g = foldl' calculate 1 [1..g]
  where
    calculate y x = y*(g+x) `quot` x

latticePath''' m n = foldl' calculate 1 [1..m]
  where
    calculate y x = y*(n+x) `quot` x

{-
main :: IO()
main = putStrLn $ show $ (`mod` (10^9+7)) $ latticePath''' 500 500
-}

stringToInt :: String -> Int
stringToInt = read

stringToInts :: String -> [Integer]
stringToInts s = read <$> (words s)


main :: IO()
main = do
  n <- fmap stringToInt getLine
  replicateM_ n (do
                    [a, b] <- fmap stringToInts getLine
                    print $ (`mod` (10^9+7)) $ latticePath''' a b)


