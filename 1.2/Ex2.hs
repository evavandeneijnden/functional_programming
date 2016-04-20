database :: [(name,age,gender,place_of_residence)]

name :: (a,b,c,d) -> a
name (a,b,c,d) = a

age :: (a,b,c,d) -> b
age (a,b,c,d) = b

gender :: (a,b,c,d) -> c
gender (a,b,c,d) = c

place_of_residence :: (a,b,c,d) -> d
place_of_residence (a,b,c,d) = d

incAgeRecur :: [(a,b,c,d)] -> Int -> [(a,b,c,d)]
incAgeRecur (p:persons) inc | length persons == 0 = [(name p,age p+inc, gender p, place_of_residence p]
                            | otherwise = incAgeRecur [p] inc : (incAgeRecur persons inc)

incAgeSingle :: (a,b,c,d) -> Int -> (a,b,c,d)
incAgeSingle = (a, b+inc,c,d)

incAgeHigh :: [(a,b,c,d)] -> Int -> [(a,b,c,d)]
incAgeHigh persons inc = map incAgeSingle persons
