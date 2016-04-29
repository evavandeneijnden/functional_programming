import FPPrac.Trees
import Ex1

-- data Tree1a = Leaf1a Int
--                     | Node1a Int Tree1a Tree1a

cutOffAt :: Tree1a -> Int -> Tree1a
cutOffAt (Leaf1a x) n
                    | n >= 0 = Leaf1a x
                    | otherwise = error "cutOffAt called with negative depth"

cutOffAt (Node1a x t1 t2) n
                            | n == 0 = Leaf1a x
                            | otherwise = Node1a x (cutOffAt t1 (n-1)) (cutOffAt t2 (n-1))
