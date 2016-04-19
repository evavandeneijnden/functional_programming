r :: Num a => a -> a -> [a]
r start dif = start : r (start+dif) dif

r1 :: Num a => [a] -> Int -> a
r1 r index = r !! index

total :: Num a => [a] -> Int -> Int -> a
total r i j | i == j = r !! i
            | otherwise = r !! i + total r (i+1) j
