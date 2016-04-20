rowlengthequal :: Eq a => [[a]] -> Bool
rowlengthequal (xs:xss) | xss == [] = True
                        | (length xs) == (length (head xss)) = (rowlengthequal xss)
                        | otherwise = False

rowtotals :: (Eq a, Num a) => [[a]] -> [a]
rowtotals (xs:xss)  | xss == [] = [(sum xs)]
                    | otherwise = (sum xs) : (rowtotals xss)

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose matrix = (map head matrix): transpose (map tail matrix)

columntotals :: (Eq a,Num a) => [[a]] -> [a]
columntotals matrix = rowtotals (transpose matrix)
