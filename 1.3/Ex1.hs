module Ex1 where

            import FPPrac.Trees

            data Tree1a = Leaf1a Int
                        | Node1a Int Tree1a Tree1a

            pp1a :: Tree1a -> RoseTree
            pp1a (Leaf1a i) = RoseNode (show i) []
            pp1a (Node1a i t1 t2) = RoseNode (show i) [pp1a t1, pp1a t2]

            t1a = Node1a 36 (Node1a 35 (Leaf1a 33) (Leaf1a 32)) (Node1a 31 (Leaf1a 30) (Leaf1a 29))

            data Tree1b = Leaf1b (Int, Int)
                        | Node1b (Int, Int) Tree1b Tree1b

            pp1b :: Tree1b -> RoseTree
            pp1b (Leaf1b (i1,i2)) = RoseNode ("("++(show i1)++","++(show i2)++")") []
            pp1b (Node1b (i1,i2) t1 t2) = RoseNode ("("++(show i1)++","++(show i2)++")") [pp1b t1, pp1b t2]

            t1b = Node1b (1,2) (Leaf1b (3,4)) (Leaf1b (5,6))

            data Tree1c = Leaf1c Int
                        | Node1c Tree1c Tree1c

            pp1c :: Tree1c -> RoseTree
            pp1c (Leaf1c i) = RoseNode (show i) []
            pp1c (Node1c t1 t2) = RoseNode "" [pp1c t1, pp1c t2]

            t1c = Node1c (Node1c (Leaf1c 21) (Leaf1c 21)) (Node1c (Leaf1c 21) (Leaf1c 21))

            data Tree1d = Leaf1d (Int, Int)
                        | Node1d [Tree1d]

            pp1d :: Tree1d -> RoseTree
            pp1d (Leaf1d (i1,i2)) = RoseNode ("("++(show i1)++","++(show i2)++")") []
            pp1d (Node1d t_list) = RoseNode "" [pp1d n1 | n1 <- t_list]

            t1d = Node1d [Node1d [Leaf1d (1,1)], Node1d [Leaf1d (1,1)], Node1d [Leaf1d (1,1)]]
