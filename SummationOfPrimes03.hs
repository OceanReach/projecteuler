import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.List (foldl1)

sieveUA :: Int -> UArray Int Bool
sieveUA top = runSTUArray $ do
    let m = (top-1) `div` 2
        r = floor . sqrt $ fromIntegral top + 1
    sieve <- newArray (1,m) True      -- :: ST s (STUArray s Int Bool)
    forM_ [1..r `div` 2] $ \i -> do
      isPrime <- readArray sieve i
      when isPrime $ do               -- ((2*i+1)^2-1)`div`2 == 2*i*(i+1)
        forM_ [2*i*(i+1), 2*i*(i+2)+1..m] $ \j -> do
          writeArray sieve j False
    return sieve


sumPs :: Int -> Int
--sumPs top = foldl1 (+) $ 2:[i*2+1 | (i, True) <- assocs $ sieveUA top]
sumPs m = sum $ takeWhile (<= m) $ primesSTE 

stringToInt :: String -> Int
stringToInt = read

main :: IO ()
main = do
  n <- fmap stringToInt getLine
  replicateM_ n (do
                    m <- fmap stringToInt getLine
                    print $ sumPs m)

primesSTE = 2 : ops
    where                  
    ops = sieve 3 9 ops []                                -- odd primes
    sieve x q ~(p:pt) fs = 
        ([x,x+2..q-2] `minus` joinST [[y+s, y+2*s..q] | (s,y) <- fs])
        ++ sieve (q+2) (head pt^2) pt
                   ((++ [(2*p,q)]) [(s,q-rem (q-y) s) | (s,y) <- fs])

    minus (x:xs) (y:ys) = case (compare x y) of
      LT -> x : minus xs (y:ys)
      EQ -> minus xs ys
      GT -> minus (x:xs) ys

    minus xs _ = xs
 
joinST (xs:t) = (union xs . joinST . pairs) t
    where
    pairs (xs:ys:t) = union xs ys : pairs t
    pairs t         = t

    union (x:xs) (y:ys) = case (compare x y) of
      LT -> x : union xs (y:ys)
      EQ -> x : union xs ys
      GT -> y : union (x:xs) ys

    union xs [] = xs
    union [] ys = ys
    
joinST []     = []

