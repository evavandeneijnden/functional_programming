pyth :: Int -> [(Int,Int,Int)]
pyth n = [(a,b,c) | a <- [0..n], b <- [0..n], c <- [0..n], a^2+b^2 == c^2]

pyth' :: Int -> [(Int,Int,Int)]
pyth' n = [(a,b,c) | a <- [0..n], b <- [a..n], c <- [b..n], a^2+b^2 == c^2 && gcd a b == 1]
