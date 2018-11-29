import Control.Monad
--import qualified Data.List as DL
import qualified Data.Vector as DV

collatz n = (if even n then n else 3*n + 1) `quot` 2

lengthOfcollatz 0 = 0
lengthOfcollatz n = length $ takeWhile (/= 1) $ iterate collatz n

collatzSeqMost :: [Int]
collatzSeqMost = 1:make 1 1 2
  where
    make m l i
     | newL >= l = i:make i newL (i+1)
     | otherwise = make m l (i+1)
       where
         newL = lengthOfcollatz i

collatzSeqMost' :: DV.Vector Int
collatzSeqMost' = DV.fromList $ take 10000007 $ 0:make 1 0 2
  where
    make m l i
      | newL >= l = i:make i newL (i+1)
      | otherwise = m:make m l (i+1)
        where
          newL = lengthOfcollatz i
{-
collatzSeq n = DV.fromList $ DL.takeWhile (<= n) $ 0:make 1 0 2
  where
    make m l i
      | newL >= l = i:make i newL (i+1)
      | otherwise = make m l (i+1)
        where
          newL = lengthOfcollatz i
-}
collatzSeq'' :: Int -> DV.Vector Int
collatzSeq'' n = DV.generate n lengthOfcollatz
 
collatzSeq' :: Int ->  DV.Vector Int
collatzSeq' n = czs
  where
    czs :: DV.Vector Int
    czs = DV.fromList $ 0:1:[make x x 0 | x <- [2..n]]
    make m i c =
      case i < m of
        True -> c + czs DV.! i     --之前的步长是已经计算过的了因此,直接加上就行
        False -> let j = collatz i in
                   make m j (c+1)
{-
data Tree = Leaf | Node Tree Integer Tree
  deriving Show



treeFromList xs = make xs l
  where
    l = length xs
    make [] _ = Leaf
    make [x] _ = Node Leaf x Leaf
    make xs l = Node leftTree x rightTree
      where
        med = l `quot` 2
        leftTree = make ls med
        rightTree = make rs (l-med)
        (ls, x:rs) = splitAt med xs
-}
{-
treeFind (Node l x r) v =
  case mi of
    dl -> tree
  (Node _ lx _) = l
  (Node _ rx _) = r
  dl = abs $ v - lx
  dr = abs $ v - rx
  dc = abs $ v - x
  mi = dl `min` dr `min` dc
  -}  


{-考虑将列表中转换为二叉树然后再进行搜索-}

longestChain n = sel 0 $ collatzSeqMost
  where
    sel m (x:xs) =
      case compare x n of
        LT -> sel x xs
        _ -> m




stringToInt :: String -> Int
stringToInt = read


main :: IO ()
main = do
  n <- fmap stringToInt getLine
  replicateM_ n (do
                    m <- fmap stringToInt getLine
                    print $ longestChain m)


{-
这个版本更慢
main :: IO ()
main = do
  n <- fmap stringToInt getLine
  replicateM_ n (do
                    m <- fmap stringToInt getLine
                    print $ collatzSeqMost' DV.! (m-1))

-}

