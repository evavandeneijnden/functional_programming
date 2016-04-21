sieve :: [Int]
sieve = sieve' [2..]

sieve' :: [Int] -> [Int]
sieve' (x:xs) = x : (sieve' $ multiple_filter x xs)
    where
      multiple_filter :: Int -> [Int] -> [Int]
      multiple_filter n list = (filter (not.(==0).(`mod` n)) list)

inSortedList :: Int -> [Int] -> Bool
inSortedList n (x:xs) | x == n = True
                | x > n = False
                | otherwise = (inSortedList n xs)

isPrime :: Int -> Bool
isPrime n = inSortedList n sieve

firstNPrimes :: Int -> [Int]
firstNPrimes n = take n sieve

primesSmallerThan :: Int -> [Int]
primesSmallerThan n = filter (< n) finiteSieve
                      where
                        finiteSieve = firstNPrimes n

divisors :: Int -> [Int]
divisors n = filter ((==0) . (n `mod`)) [1..n]

isPrime2 :: Int -> Bool
isPrime2  n = length (divisors n) == 2
