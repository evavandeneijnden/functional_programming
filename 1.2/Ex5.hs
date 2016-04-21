increasing :: (Num a, Ord a) => [a] -> Bool
increasing [] = True
increasing [x] = True
increasing (x:y:xs) | x < y = increasing (y:xs)
                    | otherwise = False

weakIncr :: [Int] -> Bool
weakIncr [] = True
weakIncr list | tail list == [] = True
              | (sum (init list)) < (length (init list) * (last list)) = weakIncr (init list)
              | otherwise = False
