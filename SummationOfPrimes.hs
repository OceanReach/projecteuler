import Control.Monad
import Data.List (inits, tails, foldl', foldl1)
import Data.Array.Unboxed

data STtree = Leaf Int | Node STtree Int STtree
  deriving Show

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n
  | even n = False
  | otherwise = null $ dropWhile cond ns
  where
    n' = truncate $ sqrt $ fromIntegral n + 0.0
    ns = [2,3..n']

    cond x = n `mod` x /= 0

{-
sumPs :: Int -> Int
sumPs k = foldl1 sel ns
  where
    ns = 2:[3,5..k]

    sel y x = if isPrime x then y + x else y
-}

stringToInt :: String -> Int
stringToInt = read

main :: IO ()
main = do
  n <- fmap stringToInt getLine
  replicateM_ n (do
                    m <- fmap stringToInt getLine
                    print $ sumPs m)


primesTo :: Int -> [Int]
primesTo m = 2:sieve [3,5..m]
  where
    sieve (p:xs)
      | p*p > m = p : xs              --sqrt m前的数
      | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p..])   --p*p, p*p + 2*p, p*p + 4*p.. we don't need these numbers

    minus (x:xs) (y:ys) = case (compare x y) of
      LT -> x : minus xs (y:ys)
      EQ -> minus xs ys
      GT -> minus (x:xs) ys

    minus xs _ = xs

sumPs :: Int -> Int
--sumPs = sum . primesTo
--sumPs m = sum $ takeWhile (<=m) $ psSAGE
sumPs m = sum $ takeWhile (<=m) $ primesSAE

    
psSAGE :: [Int]
psSAGE = 2 : [n | (r:q:_, fs) <- (zip . tails . (2:) . map (^2) <*> inits) psSAGE,
                  (n,True)    <- assocs (
                                   accumArray (\_ _ -> False) True (r+1, q-1) 
                        [(m,()) | p <- fs, let s = (r+p)`div`p*p, 
                                  m <- [s,s+p..q-1]] :: UArray Int Bool )]

--primesSAE = 2 : sieve 2 4 (tail primesSAE) (inits primesSAE)
primesSAE = (2:) . (sieve 2 4 . tail <*> inits) $ primesSAE
  where
  sieve r q ps (fs:ft) = [n | (n,True) <- assocs (
         accumArray (\ _ _ -> False) True (r+1,q-1)
                    [(m,()) | p <- fs, let s = p * div (r+p) p,
                              m <- [s,s+p..q-1]] :: UArray Int Bool )]
      ++ sieve q (head ps^2) (tail ps) ft



{-
buildSTtree :: [Int] -> STtree
buildSTtree xs = go xs n
  where
    n = length xs

    go [n] _ = if isPrime n then (Leaf n) else Leaf 0
    go ns l = Node leftPart result rightPart
      where
        leftPart = go ls dl
        rightPart = go rs dr
        result = merge leftPart rightPart

        (ls, rs) = splitAt p ns
        p = l `div` 2
        dl = p
        dr = l - p

        merge (Leaf a) (Leaf b) = a + b
        merge (Node _ a _) (Leaf b) = a + b
        merge (Leaf a) (Node _ b _) = a + b
        merge (Node _ a _) (Node _ b _) = a + b
  


sumPs :: Int -> Int
sumPs k = result
  where
    ns = 2:[3,5..k]
    (Node _ result _) = buildSTtree ns
-}
