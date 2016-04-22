import Data.List

bsort_pass :: Ord a => [a] -> [a]
bsort_pass [] = []
bsort_pass [x] = [x]
bsort_pass [x,y]      | x > y = [y,x]
                      | otherwise = [x,y]
bsort_pass (x:y:ys)   | x > y = y : bsort_pass (x:ys)
                      | otherwise = x : bsort_pass (y:ys)


bsort :: Ord a => [a] -> [a]
bsort [] = []
bsort list = bsort (init temp) ++ [last temp]
            where
              temp = bsort_pass list

minmax :: Ord a => [a] -> [a]
minmax []  = []
minmax [x] = [x]
minmax list = min_element : (minmax (list \\ [max_element, min_element])) ++ [max_element]
            where
              max_element = maximum list
              min_element = minimum (list \\ [max_element])


myinsert :: (Ord a) => [a] -> a -> [a]
myinsert [] element = [element]
myinsert (x:xs) element | x >= element = element:x:xs
                        | otherwise = x: (myinsert xs element)

isort :: Ord a => [a] -> [a]
isort = foldl myinsert []

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge [] list2 = list2
merge list1 [] = list1
merge (x:xs) (y:ys) | (x < y) = (x : (merge xs (y:ys)))
                    | otherwise = (y : (merge (x:xs) ys))

-- Deze werkt nog niet!
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort list = merge (mergesort list1) (mergesort list2)
                where
                    splitting_point = round (listlength / 2)
                    listlength = length list
                    (list1, list2) = splitAt splitting_point list

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort (x:xs) = qsort([x1 | x1 <- xs, x1 < x]) ++ [x] ++ qsort([x2 | x2 <- xs, x2 >= x])
