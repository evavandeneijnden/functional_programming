import FPPrac.Trees
import Ex1

binMirror :: Tree1a -> Tree1a
binMirror (Leaf1a i) = Leaf1a i
binMirror (Node1a i t1 t2) = Node1a i (binMirror t2) (binMirror t1)

binMirror' :: Tree1d -> Tree1d
binMirror' (Leaf1d (i1,i2)) = Leaf1d (i2,i1)
binMirror' (Node1d t_list) = Node1d (reverse (map binMirror' t_list))
