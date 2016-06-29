import Debug.Trace
import FPPrac.Trees
import RBT

------------------------------------- Insertion ------------------------------------------

insert :: RBT -> Int -> RBT
insert (Node color value leftTree rightTree) new_value  | new_value < value = Node color value (insert leftTree new_value) rightTree
                                                        | otherwise = Node color value leftTree (insert rightTree new_value)
insert (Leaf colour) new_value = Node Red new_value (Leaf Black) (Leaf Black)

fixRed :: RBT -> RBT
-- Goes down the tree to start upwards-fixing
fixRed (Node colour x t1 t2) = fixRedRoot (fixRed' newTree)
                        where
                            newTree = (Node colour x (fixRedChild t1) (fixRedChild t2))
fixRed (Leaf colour) = Leaf colour

fixRedChild :: RBT -> RBT
fixRedChild (Node colour x t1 t2) = fixRed' newTree
                        where
                            newTree = (Node colour x (fixRedChild t1) (fixRedChild t2))
fixRedChild (Leaf colour) = Leaf colour

fixRedRoot :: RBT -> RBT
-- case 1
fixRedRoot (Node Red x t1 t2) = Node Black x t1 t2
-- Don't fix what ain't broken aye?
fixRedRoot tree = tree

-- Fixes upwards
fixRed' :: RBT -> RBT
-- case 2
fixRed' (Node Black b
            (Node Red r1
                (Node Red gc1 ggc1 ggc2)
                gc2
            )
            (Node Red r2
                gc3
                gc4
            )
        ) =
        (Node Red b
            (Node Black r1
                (Node Red gc1 ggc1 ggc2)
                gc2
            )
            (Node Black r2
                gc3
                gc4
            )
        )
fixRed' (Node Black b
            (Node Red r1
                gc1
                (Node Red gc2 ggc1 ggc2)
            )
            (Node Red r2
                gc3
                gc4
            )
        ) =
        (Node Red b
            (Node Black r1
                gc1
                (Node Red gc2 ggc1 ggc2)
            )
            (Node Black r2
                gc3
                gc4
            )
        )
fixRed' (Node Black b
            (Node Red r1
                gc1
                gc2
            )
            (Node Red r2
                (Node Red gc3 ggc1 ggc2)
                gc4
            )
        ) =
        (Node Red b
            (Node Black r1
                gc1
                gc2
            )
            (Node Black r2
                (Node Red gc3 ggc1 ggc2)
                gc4
            )
        )
fixRed' (Node Black b
            (Node Red r1
                gc1
                gc2
            )
            (Node Red r2
                gc3
                (Node Red gc4 ggc1 ggc2)
            )
        ) =
        (Node Red b
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
fixRed' (Node Black b
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
fixRed' (Node Black b
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
fixRed' (Node Black b
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
fixRed' (Node Black b
            c1
            (Node Red r1
                gc1
                (Node Red gc2 ggc1 ggc2)
            )
        ) = trace "Applying case 3.4"
        (Node Black r1
            (Node Red b c1 gc1)
            (Node Red gc2 ggc1 ggc2)
        )

-- Don't fix what ain't broken aye?
fixRed' tree = tree

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
fixed' (Node Red _
            (Leaf Black)
            (Leaf Black)
        ) = True
fixed' _ = False


balancedInsert :: RBT -> Int -> RBT
balancedInsert tree value = fixRed (insert tree value)

------------------------------------- Deletion ------------------------------------------

leftmostValue :: RBT -> Int
-- Leaf, thus no left-children, thus itself
leftmostValue (Leaf c) = error "Value not found in tree"
-- Node without a left child-node, thus itself
leftmostValue (Node _ v (Leaf _) _) = v
-- Any other node, thus left child
leftmostValue (Node _ _ t1 _) = leftmostValue t1

removeValue :: RBT -> Int -> RBT
removeValue (Leaf c) = error "Value not found in tree"
removeValue (Node c v leftsub rightsub) remove  | v == remove && replacement == (Leaf Grey) = fixGrey (Node c successor leftsub replacement)
                                                | v == remove && replacement /= (Leaf Grey) = Node c successor leftsub replacement
                                                | v < remove = Node c v (removeValue leftsub remove) rightsub
                                                | v >= remove = Node c v leftsub (removeValue rightsub)
                                            where
                                                successor = leftmostValue rightsub
                                                replacement = removeSmallest rightsub

removeSmallest :: RBT -> RBT
-- case theta is red
removeSmallest (Node Red v (Leaf _) rightsub) = rightsub
-- case theta is black, right child is red
removeSmallest (Node Black v (Leaf _) (Node Red v2 leftsub rightsub)) = (Node Black v2 leftsub rightsub)
-- case theta is black with two leafs as children
removeSmallest (Node Black v (Leaf _) (Leaf _)) = Leaf Grey
-- find the smallest one
removeSmallest (Node _ _ leftsub _) = removeSmallest leftsub



 ------------------------------------- Testing ------------------------------------------

testRBT = (Node Black 6 (Node Red 5 (Leaf Black) (Leaf Black)) (Node Red 7 (Leaf Black) (Leaf Black)))

test0 = showRBTree $ showRBT $ testRBT

test1 = showRBTree   (showRBT
                        (balancedInsert (balancedInsert (balancedInsert testRBT 9) 36) 8)
                    )

test2 = showRBTree   (showRBT
                        (leftmostValue testRBT)
                    )
