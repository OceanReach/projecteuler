import Data.Bits ((.&.), shiftR)
import Control.Monad (forM, replicateM_)
import qualified Data.Vector as DV

collatz n = (if n .&. 1 == 0 then n else 3*n + 1) `shiftR` 1


lengthOfcollatz 0 = 0
lengthOfcollatz n = length $ takeWhile (/= 1) $ iterate collatz n

collatzChain :: Int -> DV.Vector Int
collatzChain n = czs
  where
    czs :: DV.Vector Int
    czs = DV.fromList $ 0:0:[make x x 0 | x <- [2..n]]

    make i v c =
      case compare v i of
        LT -> c + czs DV.! v
        _ -> let nv = collatz v in
               make i nv (c+1)


collatzMaxIndex :: Int -> DV.Vector Int
collatzMaxIndex n = DV.fromList $ lst3 $ until limit process (0,[2..n],[1])
  where
    lst3 (_,_,c) = c
    limit (_,xs,_) = null xs
    process (l, (x:xs), ys@(y:_)) = let newL = lengthOfcollatz x in
                                      if newL >= l
                                      then (newL, xs,x:ys)
                                      else (l, xs,ys)

--collatzMaxIndex' :: Int -> DV.Vector Int

lastMax :: DV.Vector Int -> Int
lastMax vi = DV.maxIndexBy cond vi
  where
    cond a b = if a <= b then LT else GT


stringToInt :: String -> Int
stringToInt = read


binaryFind :: Int -> DV.Vector Int -> Int
binaryFind it vi = go 0 (l-1)
  where
    l = DV.length vi
    go left right
      | right - left == 1 = sel left right
      | otherwise = let med = (left + right) `shiftR` 1
                        mdv = vi DV.! med in
                      case compare it mdv of
                        LT -> go med right
                        GT -> go left med
                        EQ -> mdv


    sel lt rt
      | it >= vl = vl
      | otherwise = vr
        where
          vl = vi DV.! lt
          vr = vi DV.! rt



main :: IO ()
main = do
  n <- fmap stringToInt getLine
  xs <- forM [1..n] (\_ -> do
                        m <- fmap stringToInt getLine
                        return m)
  let mx = maximum xs
      cmi = collatzMaxIndex mx
--      rs = (\x -> DV.head $ DV.dropWhile (> x) cmi)  <$> xs in
--      rs = (\x -> binaryFind x cmi) <$> xs in
      rs = (\x -> DV.find (<= x) cmi)  <$> xs in
    mapM_ (\r -> let (Just x) = r in print x) rs




