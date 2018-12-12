import Data.Vector (fromList, Vector, elem, (!), foldl', foldr, generate, (++))
import Control.Monad

dvElem x vx = Data.Vector.elem x vx
lsElem x lx = Prelude.elem x lx
dvFoldl' = Data.Vector.foldl'
dvFoldr = Data.Vector.foldr
lsFoldr = Prelude.foldr
lsScanr = Prelude.scanr
lsScanl = Prelude.scanl
a `dvL` b = a Data.Vector.++ b

--1905年01/01是第一个周日
--本年是平年，来年day + 1，即1906 01/01为周一(
--本年是闰年，来年day + 2, 即1909 01/01为周五

--每个月的天数对7求模
monthesModDays :: Int -> Vector Int
monthesModDays y = if leepYear y
                   then fromList $ [3,1,3,2,3,2,3,3,2,3,2,3]
                   else fromList $ [3,0,3,2,3,2,3,3,2,3,2,3]



leepYear :: Int -> Bool    
leepYear n
  | n `mod` 100 == 0 = n `mod` 400 == 0
  | otherwise = n `mod` 4 == 0


yearDates yr = dates
  where
    dates = fromList $ 0:(lsFoldr go [] years)
    years = [1905..yr-1]
    go x xs = let p = (x-1905)
                  ld = dates ! p in
                if leepYear x
                then (ld + 2) `mod` 7:xs
                else (ld + 1) `mod` 7:xs
                
      


dates0101 :: Int -> Int
dates0101 yr = (`mod` 7) $ dvFoldl' go 0 years
  where
    years = fromList [1905..yr-1]
    go r x = if leepYear x then r + 2 else r + 1


--有哪些年的0101是周日，总共有多少
--sundays :: Int -> Int
sundays yr = dvFoldl' go (0, 1) years
  where
    years = fromList [1900..yr]
    go (r, d) x = if nd > 6 then (r'+1, nd `mod` 7) else (r', nd)
      where
        (r', nd)
          | leepYear x = (r, d+2)
          | otherwise = (r, d+1)
          

--计算每个月的第一天有多少个周日并求和
--sundaysInMounth :: Int -> Int
sundaysInMounth yr = dvFoldl' go (0, 2) years
  where
    years = fromList [1901..yr]
    go (r, d) x = (r+c, nd `mod` 7)
      where
        c = if d == 0 || d > 4 then 5 else 4
        nd = if leepYear x then d + 2 else d + 1



--一年中有多少个月是以周日作为开头的，并求和
monthesSundays yr = dvFoldl' go (0, 2) years
  where
    years = fromList [1901..yr]

    go (r, fd) y = (r+c, nfd `mod` 7)
      where
        mmds = monthesModDays y
        c = dvFoldl' (\r x -> if x == 0 then r + 1 else r) 0 mds'    --统计周日的数目（为0的数目）
        nfd = if leepYear y then fd + 2 else fd + 1

        mds' = fromList [fd] `dvL` generate 11 make
        make i = let lm = mds' ! i            --lm表示上一个0101是星期几
                     hm = mmds ! i in
                   (lm+hm) `mod` 7
                   

