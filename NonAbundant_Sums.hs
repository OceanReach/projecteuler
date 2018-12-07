import Data.Vector (Vector, (!), (//), fromList, foldl', tail, head, dropWhile, null, length, generate)
import Control.Monad

dvFoldl' = Data.Vector.foldl'
dvHead = Data.Vector.head
dvTail = Data.Vector.tail
lsTail = Prelude.tail
dvDropWhile = Data.Vector.dropWhile
dvNull = Data.Vector.null
dvLength = Data.Vector.length
--dvNub as bs = as `//` bs

divisorsSum :: Int -> Int
divisorsSum x = go x 1 0 (2:[3,5..up])
  where
    up = truncate $ sqrt $ fromIntegral x
    

    go _ sum _ [] = sum
    go t sum c (n:ns)
      | t `rem` n == 0 = go (t `quot` n) sum (c+1) (n:ns)
      | c /= 0 = go t (sum * (n^(c+1) - 1) `quot` (n - 1)) 0 ns
      | otherwise = go t sum c ns


divisorsSum' :: Int -> Int
divisorsSum' x = get2 $ dvFoldl' go (1, x)  $ fromList (2:[3,5..up])
  where
    get2 (a, b) = if b > 1 then a * (b+1) - x else a - x
    up = truncate $ sqrt $ fromIntegral x

    go (s, t) n
      | t `rem` n == 0 = make s t n
      | otherwise = (s, t)
        where
          make r y u
            | y `rem` n == 0 = make r (y `quot` n) (u*n)
            | otherwise = (r*(u-1) `quot` (n-1), y)


ma :: Int
ma = 28123

isAbundant :: Int -> Bool
isAbundant x = x < divisorsSum' x

abundants ::Vector Int
abundants = fromList $ filter isAbundant [ma, ma-1..1]


abundants' :: Vector Int
abundants' = let halfMa = ma `quot` 2 in
              fromList $ filter isAbundant [halfMa, halfMa-1..1]


nonAbundants :: Vector Int
nonAbundants = let halfMa = ma `quot` 2 in
                 fromList $ filter (not . isAbundant) [halfMa, halfMa-1..1]



sumOfNonTwoAbundantsSum :: Int
sumOfNonTwoAbundantsSum = dvFoldl' catch 0 $ fromList [ma, ma-1..1]
  where
    ps = abundants
    mix = dvLength ps
    catch s x = if isPass then s + x else s
      where
        isPass = not $ ps ! h + ps ! l == x
        (h, l) = until limit process (0, mix-1)

        limit (hi, li) = hi == li || x == (ps ! hi + ps ! li)
        process (hi, li) = let sum = ps ! hi + ps ! li in
                             if sum > x
                             then (hi+1, li)
                             else (hi, li-1)




sumOfNonTwoAbundantsSum' :: Int
sumOfNonTwoAbundantsSum' = dvFoldl' catch 0 $ fromList [1..ma]
  where
    ps = abundants
    sz = dvLength ps
    
    sas = generate (ma * 2 + 1) (\_ -> False)
    sas' = sas // kv

    kv = [(ps ! i + ps ! j, True) | i <- [0..sz-1], j <- [0..i]]

    catch r t = if sas' ! t
                then r
                else r + t


main :: IO()
main = print sumOfNonTwoAbundantsSum


{-
暴力解法不可取，贼贼贼贼慢

twoAbundantsSum ::Vector Int
twoAbundantsSum = let as = abundants
                      bs = lsTail as in
                    do
                      a <- as
                      b <- bs
                      return $ a + b

sumOfNonTwoAbundantsSum :: Int
sumOfNonTwoAbundantsSum = sum $ [1..28123] \\ twoAbundantsSum
-}

