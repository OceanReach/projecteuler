import Control.Monad
import Data.Vector (Vector, (!), fromList)

numLetters::Vector String
numLetters = fromList [[], "One", "Two", "Three", "Four", "Five", "Six", "Seven","Eight","Nine","Ten","Eleven","Twelve","Thirteen","Fourteen","Fifteen","Sixteen","Seventeen","Eighteen","Nineteen","Twenty","Thirty","Forty","Fifty","Sixty","Seventy","Eighty","Ninety"] --,"Hundred","Thousand","Million","Billon"]


hund :: Int
hund = 100

thou :: Int
thou = 10^3

mill :: Int
mill = 10^6

bill :: Int
bill = 10^9

trill :: Int
trill = 10^12 

ntl :: Int -> String
ntl 0 = "Zero"
ntl n
  | n <= 20 = numLetters ! n
  | n < hund = let p = (n-20) `div` 10 in
                 if n `mod` 10 == 0
                 then numLetters ! (20+p)
                 else numLetters ! (20+p) ++ " " ++ numLetters ! (n`mod`10)

  | n < thou = let h = n `div` 100 in
                 if n `mod` hund == 0
                 then ntl h ++ " Hundred"
                 else ntl h ++ " Hundred " ++ ntl (n`mod`hund)

  | n < mill = let h = n `div` thou in
                    if n `mod` thou == 0
                    then ntl h ++ " Thousand"
                    else ntl h ++ " Thousand " ++ ntl (n `mod` thou)
  | n < bill = let h = n `div` mill in
                       if n `mod` mill == 0
                       then ntl h ++ " Million"
                       else ntl h ++ " Million " ++ ntl (n `mod` mill)
  | n < trill = let h = n `div` bill in
                   if n `mod` bill == 0
                   then ntl h ++ " Billion"
                   else ntl h ++ " Billion " ++ ntl (n `mod` bill)
  | otherwise = let h = n `div` trill in
                  if n `mod` trill == 0
                  then ntl h ++ " Trillion"
                  else ntl h ++ " Trillion " ++ ntl (n `mod` trill)
                  
                   
lengthNTL :: Int -> Int
lengthNTL = length . ntl

main :: IO ()
main = do
  n <- fmap (read :: String -> Int) getLine
  replicateM_ n (do
                    m <- fmap (read :: String -> Int) getLine
                    putStrLn $ ntl m)
