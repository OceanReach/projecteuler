import Control.Monad

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


factors :: Int -> Int
factors n
  | n'^2 == n = result - 1
  | otherwise = result
  where
    n' = truncate $ sqrt $ fromIntegral n
    result = foldl count 0 [1..n']
    count r x
      | n `mod` x /= 0 = r
      | otherwise = r+2


divCount :: Int -> Int -> [(Int, Int)]
divCount n m = count n m 1
  where
    count n m c = (n, c) : count (n `div` m) m (c+1)

 --nod' = nod * exp prime factors 的排列相当于不同的因子的组合，比如5^2*2可以是10*5, 25*2,50*1，因此n!就是factors的数量
primeFactorsation :: Int -> Int
primeFactorsation 0 = 1
primeFactorsation 1 = 1
primeFactorsation n = iter n 1 ps
  where
    n' = truncate $ sqrt $ fromIntegral n
    ps = takeWhile (<= n') primesSTE

    iter 1 nod _ = nod
    iter _ nod [] = nod * 2
    iter remain nod (n:ns) = iter remain' nod' ns
        where
          nod' = nod * exp         
          (remain', exp) = head $ dropWhile ((== 0).(`mod` n).fst) $ divCount remain n
        

--D(Nk) = D(k/2)*D(k+1) 或 D(Nk) = D((k+1)/2)*D(k) 这取决于k是否为偶数
--这里的小技巧在于，每次只更新其中一个（D(k+1)或D((k+1)/2))，留下的k又在下次使用
--而这便是D(k)或D(k/2)的由来
--在程序中我们用dn'和dn表示D(k/2)和D(K+1)或D((k+1)/2)和D(k)
--更新后的dn与上次的dn'相乘所表示的就是D(k+1)*D(k/2)
--同理dn'与上次的dn相乘所表示的就是D((k+1)/2)*D(k)
triangularFK :: Int -> Int
triangularFK k = make $ head $ dropWhile ((<= k) . fst4) $ generate (0,1,1,1)
  where
    fst4 (a, _,_,_) = a
    make (_,_,_,i) = i*(i-1) `div` 2

    generate item@(cn, dn, dn', i) = item:generate(ucn, udn, udn', i+1)
      where
        (ucn, udn, udn') = update item
        update (c, d, d', i)
          | even i = (ud*d', ud, d')
          | otherwise = (ud'*d, d, ud')
            where
              ud = primeFactorsation (i+1)
              ud' = primeFactorsation((i+1)`div`2)



stringToInt :: String -> Int
stringToInt = read

{-
main :: IO ()
main = do
  n <- fmap stringToInt getLine
  replicateM_ n (do
                    m <- fmap stringToInt getLine
                    print $ triangularFK m)
  
-}
main :: IO ()
main = print $ triangularFK 2000
