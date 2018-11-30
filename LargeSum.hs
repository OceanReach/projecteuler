import Control.Monad
import System.IO
import Control.Applicative
import Data.Char
filepath :: String
filepath = "/home/kojirosjy/Code/CoderWebsite/ProjectEuler/testLargeSum.txt"

stringToInt :: String -> Double
stringToInt = read

caculate :: Handle -> IO Double
caculate hf = do
  l <- fmap stringToInt $ hGetLine hf
  ls <- caculate hf
  return $ l + ls
  <|> return 0

sumFromFile :: IO ()
sumFromFile = do
  hf <- openFile filepath ReadMode
  ss <- fmap show $ caculate hf
  putStrLn $ filter isNumber $ take 11 ss

main :: IO ()
main = sumFromFile
