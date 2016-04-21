sieve :: [Int]
sieve = sieve' [2..]

sieve' :: [Int] -> [Int]
sieve' (x:xs) = x : (sieve' $ multiple_filter x xs)
    where
      multiple_filter :: Int -> [Int] -> [Int]
      multiple_filter n list = (filter (not.(==0).(`mod` n)) list)
