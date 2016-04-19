r :: Num a => a -> a -> [a]
r start dif = rrecur [start] start dif

rrecur :: Num a => [a] -> a -> a -> [a]
rrecur list last dif = rrecur (list ++ [last+dif]) (last+dif) dif
