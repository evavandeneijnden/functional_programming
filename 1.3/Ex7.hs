import FPPrac.Trees
import Ex1

-- data Tree1a = Leaf1a Int
--                     | Node1a Int Tree1a Tree1a


replace :: Tree1a -> Int -> [Char] -> Tree1a
replace (Leaf1a x) y [] = Leaf1a y
replace (Node1a x t1 t2) y [] = Node1a y t1 t2
replace (Leaf1a x) y path = error "Path doesn't lead to a node"
replace (Node1a x t1 t2) y (p:path)
                            | p == 'l' = Node1a x (replace t1 y path) t2
                            | p == 'r' = Node1a x t1 (replace t2 y path)
                            | otherwise = error "unknown character in path"

subTree :: Tree1a -> [Char] -> Tree1a
subTree t [] = t
subTree (Leaf1a x) path = error "Path doesn't lead to a node"
subTree (Node1a x t1 t2) (p:path)
                            | p == 'l' = t1
                            | p == 'r' = t2
                            | otherwise = error "unknown character in path"
