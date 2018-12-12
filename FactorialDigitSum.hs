departAndSum bn = fst $ until limit process (0, bn)
  where
    limit (_, n) = n == 0
    process (s, bn) = let t = bn `mod` 10 in
                        (s+t, bn `quot` 10)
