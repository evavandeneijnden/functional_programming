import FPPrac.Trees
import RBT

insert :: RBT -> Int -> RBT
insert (Node color value leftTree rightTree) new_value  | new_value < value = Node color value (insert leftTree new_value) rightTree
                                                        | otherwise = Node color value leftTree (insert rightTree new_value)
insert (Leaf colour) new_value = Node Red new_value (Leaf Black) (Leaf Black)

fixRed :: RBT -> RBT
-- case 1
fixRed (Node Red x (Node Red y yt1 yt2) t2) = Node Black x (Node Red y yt1 yt2) t2
fixRed (Node Red x t1 (Node Red y yt1 yt2)) = Node Black x t1 (Node Red y yt1 yt2)

-- case 2
fixRed (Node Black b
            (Node Red r1
                (Node Red gc1 ggc1 ggc2)
                gc2
            )
            (Node Red r2
                gc3
                gc4
            )
        ) = fixRed (
        Node Red b
            (Node Black r1
                (Node Red gc1 ggc1 ggc2)
                gc2
            )
            (Node Black r2
                gc3
                gc4
            )
        )
fixRed (Node Black b
            (Node Red r1
                gc1
                (Node Red gc2 ggc1 ggc2)
            )
            (Node Red r2
                gc3
                gc4
            )
        ) = fixRed (
        Node Red b
            (Node Black r1
                gc1
                (Node Red gc2 ggc1 ggc2)
            )
            (Node Black r2
                gc3
                gc4
            )
        )
fixRed (Node Black b
            (Node Red r1
                gc1
                gc2
            )
            (Node Red r2
                (Node Red gc3 ggc1 ggc2)
                gc4
            )
        ) = fixRed (
        Node Red b
            (Node Black r1
                gc1
                gc2
            )
            (Node Black r2
                (Node Red gc3 ggc1 ggc2)
                gc4
            )
        )
fixRed (Node Black b
            (Node Red r1
                gc1
                gc2
            )
            (Node Red r2
                gc3
                (Node Red gc4 ggc1 ggc2)
            )
        ) = fixRed (
        Node Red b
            (Node Black r1
                gc1
                gc2
            )
            (Node Black r2
                gc3
                (Node Red gc4 ggc1 ggc2)
            )
        )
-- case 3
fixRed (Node Black b
            (Node Red r1
                (Node Red gc1 ggc1 ggc2)
                gc2
            )
            c2
        ) =
        (Node Black r1
            (Node Red gc1 ggc1 ggc2)
            (Node Red b gc2 c2)

        )
fixRed (Node Black b
            (Node Red r1
                gc1
                (Node Red gc2 ggc1 ggc2)
            )
            c2
        ) =
        (Node Black gc2
            (Node Red r1 gc1 ggc1)
            (Node Red b ggc2 c2)
        )
fixRed (Node Black b
            c1
            (Node Red r1
                (Node Red gc1 ggc1 ggc2)
                gc2
            )
        ) =
        (Node Black gc1
            (Node Red b c1 ggc1)
            (Node Red r1 ggc2 gc2)

        )
fixRed (Node Black b
            c1
            (Node Red r1
                gc1
                (Node Red gc2 ggc1 ggc2)
            )
        ) =
        (Node Black r1
            (Node Red b c1 gc1)
            (Node Red gc2 ggc1 ggc2)

        )

-- Fixes upwards because it fixes itself and is matched in the root if the root is not matched.
-- If the root IS matched, it will fix itself and does not need to fix upwards.
fixRed (Node colour x t1 t2) | (fixed newTree) = newTree
                             | otherwise = fixRed (newTree)
                        where
                            newTree = (Node colour x (fixRed t1) (fixRed t2))

fixRed (Leaf colour) = Leaf colour

fixed :: RBT -> Bool
fixed (Leaf c)  | c == Black = True
                | otherwise = False
fixed (Node c _ child1 child2)    | c == Black = (fixed' child1) && (fixed' child2)
                                | otherwise = False

fixed' :: RBT -> Bool
fixed' (Leaf c) | c == Black = True
                | otherwise = False
fixed' (Node Black _ child1 child2) = (fixed' child1) && (fixed' child2)
fixed' (Node Red _
            (Node Black _ grandchild1 grandchild2)
            (Node Black _ grandchild3 grandchild4)
        ) = (fixed' grandchild1) && (fixed' grandchild2) && (fixed' grandchild3) && (fixed' grandchild4)
fixed' _ = False


balancedInsert :: RBT -> Int -> RBT
balancedInsert tree value = fixRed (insert tree value)


 --- Testing ---
testRBT = (Node Black 6 (Node Red 5 (Leaf Black) (Leaf Black)) (Node Red 7 (Leaf Black) (Leaf Black)))

test = showRBTree   (showRBT
                        (balancedInsert testRBT 9)
                    )
