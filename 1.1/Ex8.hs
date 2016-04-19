allEqual :: Ord a => [a] -> Bool
allEqual [] = True
allEqual (x:xs)     | length xs == 0 = True
                    | length xs >= 2 && x == head xs = allEqual xs
                    | length xs == 1 && x == head xs = True
                    | otherwise = False
