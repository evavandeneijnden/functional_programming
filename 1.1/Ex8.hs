allEqual :: Ord a => [a] -> Bool
allEqual [] = True
allEqual (x:xs)     | length xs == 0 = True
                    | length xs >= 2 && x == head xs = allEqual xs
                    | length xs == 1 && x == head xs = True
                    | otherwise = False


isAS :: (Eq a, Num a) => [a] -> Bool
isAS [] = True
isAS [x] = True
isAS [x,y] = True
isAS (x:y:z:zs) = (x-y) == (y - z) && isAS (y:z:zs)
