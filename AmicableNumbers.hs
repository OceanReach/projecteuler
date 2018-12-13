import Data.Vector (Vector, generate, fromList, sum, foldl')
import Data.Bits ((.&.))


dvSum = Data.Vector.sum
dvFoldl' = Data.Vector.foldl'

properDivisorsAndSum :: Int -> Int
properDivisorsAndSum bn = dvSum $ generate limit make
  where
    limit = truncate $ sqrt $ fromIntegral bn

    make 0 = 0
    make 1 = 1
    make i
      | bn `rem` i == 0 = let j = bn `quot` i in
                            if j == i
                            then i
                            else i + j
      | otherwise = 0


properDivisorsAndSum' :: Int -> Int
properDivisorsAndSum' bn = dvFoldl' prcedure sum $ fromList ns
  where
    ns = if bn .&. 1 == 0 then [2..limit] else [3,5..limit]
    limit = truncate $ sqrt $ fromIntegral bn
    sum = if isSquare bn then 1 + limit else 1
    isSquare x = x == limit

    prcedure r x = case bn `rem` x == 0 of
                     True -> r + x + bn `quot` x
                     False -> r

divisorsAndSum :: Int -> Int
divisorsAndSum bn = output $ dvFoldl' prcedure (1, bn) $ fromList $ 2:[3,5..limit]
  where
    limit = truncate $  sqrt $ fromIntegral bn

    prcedure (r, n) x
      | n `rem` x == 0 = make r (n `quot` x) (x*x)
      | otherwise = (r, n)
        where
          make s m y
            | m `rem` x == 0 = make s (m `quot` x) (y*x)
            | otherwise = let ns = (s*(y-1)) `quot` (x-1) in (ns, m)      --(y-1)/(x-1) = (p^a - 1) / (p - 1)

    output (s, m) = if m > 1
                    then s*(m+1)
                    else s     
              
            
properDivisorsAndSum'' :: Int -> Int
properDivisorsAndSum'' n = divisorsAndSum n - n





amicable :: Int -> Bool
amicable x = let dx = properDivisorsAndSum'' x
                 x' = properDivisorsAndSum'' dx in
               x /= dx && x == x'

san :: Int -> Int
san u = dvFoldl' (\r x -> if amicable x then r + x else r) 0 $ fromList [2..u]

main = print $ san 100000
