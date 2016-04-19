r :: Num a => a -> a -> [a]
r start dif = start : r (start+dif) dif
