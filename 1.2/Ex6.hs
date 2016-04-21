sublist :: Eq a => [a] -> [a] -> Bool

sublist [] [] = True
sublist xs [] = True
sublist [] ys = False
sublist (x:xs) (y:ys)   | x /= y = sublist xs (y:ys)
                        | x == y = sublistHelper xs ys
sublistHelper [] [] = True
sublistHelper xs [] = True
sublistHelper [] ys = False
sublistHelper (x:xs) (y:ys)                     | x /= y = False
                                                | x == y = sublistHelper xs ys

partSublist :: Eq a => [a] -> [a] -> Bool
partSublist [] [] = True
partSublist xs [] = True
partSublist [] ys = False
partSublist (x:xs) (y:ys)                       | x /= y = partSublist xs (y:ys)
                                                | x == y = partSublist xs ys
