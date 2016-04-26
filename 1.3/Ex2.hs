import FPPrac.Trees

data Tree1a = Leaf1a Int
            | Node1a Int Tree1a Tree1a

data Tree1b = Leaf1b (Int, Int)
            | Node1b (Int, Int) Tree1b Tree1b

treeAdd :: Tree1a -> Int -> Tree1a
treeAdd (Leaf1a i) x = Leaf1a (i+x)
treeAdd (Node1a i t1 t2) x = Node1a (i+x) (treeAdd t1 x) (treeAdd t2 x)

treeSquare :: Tree1a -> Tree1a
treeSquare (Leaf1a i) = Leaf1a (i^2)
treeSquare (Node1a i t1 t2) = Node1a (i^2) (treeSquare t1) (treeSquare t2)

mapTree :: Tree1a -> (Int -> Int) -> Tree1a
mapTree (Leaf1a i) f = Leaf1a (f i)
mapTree (Node1a i t1 t2) f = Node1a (f i) (mapTree t1 f) (mapTree t2 f)

addNode :: Tree1b -> Tree1a
addNode (Leaf1b (i1,i2)) = Leaf1a (i1+i2)
addNode (Node1b (i1,i2) t1 t2) = Node1a (i1+i2) (addNode t1) (addNode t2)

mapTreeE :: Tree1b -> ((Int, Int) -> Int) -> Tree1a
mapTreeE (Leaf1b i) f = Leaf1a (f i)
mapTreeE (Node1b i t1 t2) f = Node1a (f i) (mapTreeE t1 f) (mapTreeE t2 f)
