myfilter :: Eq a => (a -> Bool) -> [a] -> [a]
myfilter func (x:xs)    | length xs == 0 && func x == True = [x]
                        | length xs == 0 && func x == False = []
                        | func x == True = x : myfilter func xs
                        | func x == False = myfilter func xs
