--module MaximumPathSumI (
module Main(
  module Control.Monad,
  module Data.Vector,
  module System.IO,
--  System.IO (openFile, hGetLine, IOMode.ReadMode)
  main,
  )where


import Control.Monad (forM, replicateM_, join)
import Data.Vector (Vector, (!), fromList, head, tail, null, length, zipWith, init)
import System.IO (openFile, hGetLine, IOMode(ReadMode), hGetContents)



dvHead = Data.Vector.head
dvTail = Data.Vector.tail
dvZipWith = Data.Vector.zipWith
dvInit = Data.Vector.init
dvNull = Data.Vector.null
dvLength = Data.Vector.length


filepath :: String
filepath = "/home/kojirosjy/Code/CoderWebsite/ProjectEuler/problem18_02.txt"


type VI = Vector Int
type VVI = Vector VI


stringToInt :: String -> Int
stringToInt = read

stringToVI :: String -> VI
stringToVI s = fromList $ read <$> (words s)

{-               
getTriangel :: IO VVI
getTriangel = do
  hd <- openFile filepath ReadMode
  n <- fmap (read::String -> Int) $ hGetLine hd
  vvi <- forM (fromList [1..n]) (\_ -> fmap stringToVI $ hGetLine hd)
  return vvi
-}

getTriangel' = do
  hd <- openFile filepath ReadMode
  vvi_t <- hGetContents hd
  let vvi_t' = lines vvi_t
      change = fromList . map (read::String -> Int) . words
      vvi = fromList $ change <$> vvi_t' in
    return vvi
  

tri1 :: VVI
tri1 = fromList $ fromList <$> [[3],[7,4],[2,4,6],[8,5,9,3]]



--暴力解法
--0
--0  1
--0 1  2


mpathsum :: VVI -> Int
mpathsum tri = go h 0 tri'
  where
    h =  dvHead $ dvHead tri
    tri' = dvTail tri
    go n i xs
      | dvNull xs = n
      | otherwise = goLeft `max` goRight
      where
        hx = dvHead xs
        rx = dvTail xs
        goLeft = n + go (hx ! i) i rx
        goRight = n + go (hx ! (i+1))  (i+1) rx


--自底向上
mpathsum' :: VVI -> Int
mpathsum' tri = dvHead $ go tri n
  where
    n = dvLength tri

    go xss 1 = dvHead xss
    go xss r = let hx = dvHead xss
                   rx = dvTail xss
                   cs = go rx (r-1) in
                      dvZipWith (+) hx $ dvZipWith max (dvInit cs) (dvTail cs)


main :: IO ()
main = do
  tri <- getTriangel'
  print $ mpathsum' tri
    
  


{-
优先选择大的
-}
