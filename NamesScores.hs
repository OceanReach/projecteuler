import Control.Monad
import System.IO
import Data.Map.Strict ((!), fromList, Map)
import Data.Char
import Data.List (sort)

alphabet :: Map Char Int
alphabet = fromList $ zip "abcdefghijklmnopqrstuvwxyz" [1..]

alphaQuery :: Char -> Int
alphaQuery x = let x' = toLower x in
                 alphabet ! x'

filepath :: String
filepath = "problem022.txt"


nameTovalue :: String -> Int
nameTovalue str = sum $ alphaQuery <$> str

splitBy :: String -> [String]
splitBy xs = foldr go [[]] xs'
  where
    xs' = tail xs     --去掉首部的引号

    go n ns
      | null $ head ns = [[n]]
      | otherwise = let item = head ns
                        h = head item in
                      case n == '"' of
                        True -> if h == ','
                                then [n]:(drop 1 item):(tail ns)
                                else (n:item):(tail ns)

                        False -> if h == '"'                          --去掉之前的引号
                                 then (n:tail item):(tail ns)
                                 else (n:item):(tail ns)

nvalue = do
  hd <- openFile filepath ReadMode
  ss <- fmap splitBy $ hGetLine hd
  let ss' = sort ss
      ssi = zip ss' [1..]
      v = foldl (\r (s, i) -> r + (i * nameTovalue s)) 0 ssi in
    return v
      

main = do
  nv <- nvalue
  print $ nv


