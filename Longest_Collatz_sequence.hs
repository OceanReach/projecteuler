import Control.Monad
import qualified Data.Vector as DV



collatz :: Int -> Int
collatz n
  | even n = n `div` 2
  | otherwise = 3 * n + 1


lengthOfCollatzSeq :: Int -> Int
lengthOfCollatzSeq 0 = 0
lengthOfCollatzSeq n = length $ takeWhile (/= 1) $ iterate collatz n


{-可不可以生成一个一定范围内最长的collatz chain
然后根据所给的数找到列表中小于它的最大的一个数这即是最终答案-}

longestCollatzSeq :: Int -> Int
longestCollatzSeq n = find (1,1) [3,5..]
  where
    find it@(m, l) (x:xs)
      | x > n = m
      | otherwise = let newL = lengthOfCollatzSeq x in
                      case compare newL l of
                        GT -> find (x, newL) xs
                        _ -> find it xs


collatz' n = (if even n then n else 3*n+1) `quot` 2

lengthOfCollatzSeq' 0 = 0
lengthOfCollatzSeq' n = length $ takeWhile (/= 1) $ iterate collatz' n

longestCollatzSeq' = go (1,1) 3
  where
    go ml@(m, l) i n
      | i > n = m
      | otherwise = let newL = lengthOfCollatzSeq' i in
                      case compare newL l of
                        LT -> go ml (i+1) n
                        _ -> go (i, newL) (i+1) n

collatzSeqMost :: [Int]
collatzSeqMost = 1:make 1 1 2
  where
    make m l i
     | newL >= l = i:make i newL (i+1)
     | otherwise = make m l (i+1)
     where
       newL = lengthOfCollatzSeq' i


longestCollatzSeq'' n = fst $ until limit process (1, xs)
  where
    xs = collatzSeqMost
    limit (m, (x:_)) = x > n
    process (m, (x:xs)) = (x, xs)


longestCollatzSeq''' :: Int -> Int
longestCollatzSeq''' n = DV.maxIndexBy cond czs
  where
    cond a b = if a <= b then LT else GT
    czs = DV.generate n lengthOfCollatzSeq'


longestCollatzSeq'''' :: Int -> Int
longestCollatzSeq'''' n = DV.maxIndexBy cond vector
  where 
    cond a b = if a <= b then LT else GT
    vector = DV.fromList $ 0 : 1 : [make x x 0 | x <- [2..n]]

    make m i c =
      case i < m of
        True  -> c + vector DV.! i
        False -> let j = if even i then i `div` 2 else 3*i + 1
                 in make m j (c+1)
    
stringToInt :: String -> Int
stringToInt = read


main :: IO ()
main = do
  n <- fmap stringToInt getLine
  replicateM_ n (do
                    m <- fmap stringToInt getLine
                    print $ longestCollatzSeq''' m)



