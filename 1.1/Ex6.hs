mylength :: [a] -> Int
mylength list   | null list = 0
                | otherwise = mylength (tail list) + 1

mysum :: [Double] -> Double
mysum list      | mylength list == 0 = 0
                | otherwise = mysum (tail list) + head list

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (listhead:listtail) = myreverse listtail ++ [listhead]

mytake :: [a] -> Int -> [a]
mytake [] num = []
mytake list 0 = []
mytake list num = head list : mytake (tail list) (num-1)

myelem :: Eq a => [a] -> a -> Bool
myelem [] el = False
myelem (x:xs) el                | x == el = True
                                | otherwise = myelem xs el

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs

mymaximum :: Ord a => [a] -> a
mymaximum (x:xs)                | mylength xs == 0 = x
                                | x < head xs = mymaximum xs
                                | x >= head xs = mymaximum ([x] ++ tail xs)

myzip :: [a] -> [a] -> [(a,a)]
myzip [] list2 = []
myzip list1 [] = []
myzip (x1:xs1) (x2:xs2) = [(x1,x2)] ++ (myzip xs1 xs2)
