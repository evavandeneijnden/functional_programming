myfilter :: Eq a => (a -> Bool) -> [a] -> [a]
myfilter func (x:xs)    | length xs == 0 && func x == True = [x]
                        | length xs == 0 && func x == False = []
                        | func x == True = x : myfilter func xs
                        | func x == False = myfilter func xs

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith func (x:xs) (y:ys)                    | length xs == 0 || length ys == 0 = [func x y]
                                                | otherwise = func x y : myZipWith func xs ys

myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl function base [] = base
myfoldl function base (x:xs) = myfoldl function (function base x) xs

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr function base [] = base
myfoldr function base (x:xs) = function x (foldr function base xs)
