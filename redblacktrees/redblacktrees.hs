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

-- Synonym for removeValue
delete :: RBT -> Int -> RBT
delete tree value = removeValue tree value

removeValue :: RBT -> Int -> RBT
removeValue (Leaf c) remove = error "Value not found in tree"
removeValue (Node c v leftsub rightsub) remove  | v == remove && replacement == (Leaf Grey) = fixGrey (Node c successor leftsub replacement)
                                                | v == remove && replacement /= (Leaf Grey) = Node c successor leftsub replacement
                                                | v < remove = Node c v leftsub (removeValue rightsub remove)
                                                | v >= remove = Node c v (removeValue leftsub remove) rightsub
                                            where
                                                successor = leftmostValue rightsub
                                                replacement = removeSmallest rightsub

removeSmallest :: RBT -> RBT
-- case theta is red
removeSmallest (Node Red v (Leaf _) rightsub) = rightsub
-- case theta is black, right child is red
removeSmallest (Node Black v (Leaf _) (Node Red v2 leftsub rightsub)) = (Node Black v2 leftsub rightsub)
-- case theta is black with two leafs as children
removeSmallest (Node Black v (Leaf _) (Leaf _)) = trace "Inserting grey leaf" Leaf Grey
-- find the smallest one
removeSmallest (Node c v leftsub rightsub) = Node c v (fixGrey (removeSmallest leftsub)) rightsub

fixGrey :: RBT -> RBT
fixGrey (Node Black p g (Node colourS s l r))
                            | (colour g) == Grey && colourS == Black && (colour l) == Black && (colour r) == Black = fixCase1 tree
                            | (colour g) == Grey && colourS == Black && (colour l) ==  Red = fixCase2 tree
                            | (colour g) == Grey && colourS == Black && (colour r) == Red = fixCase4 tree -- Colour of l has to be black, or it would have mached above
                            | (colour g) == Grey && colourS == Red = fixCase5 tree -- colours of l and r need to be black, otherwise the tree is too broken
                    where
                        tree = (Node Black p g (Node colourS s l r))
fixGrey (Node Black p (Node colourS s l r) g) -- mirrored version of above patterns
                            | (colour g) == Grey && colourS == Black && (colour l) == Black && (colour r) == Black = fixCase1 tree
                            | (colour g) == Grey && colourS == Black && (colour r) ==  Red = fixCase2 tree
                            | (colour g) == Grey && colourS == Black && (colour l) == Red = fixCase4 tree -- Colour of l has to be black, or it would have mached above
                            | (colour g) == Grey && colourS == Red = fixCase5 tree -- colours of l and r need to be black, otherwise the tree is too broken
                    where
                        tree = (Node Black p (Node colourS s l r) g)
fixGrey (Node Red p g (Node colourS s l r))
                            | (colour g) == Grey && (colour l) == Red = fixCase2 tree
                            | (colour g) == Grey && (colour l) == Black = fixCase3 tree
                    where
                        tree = (Node Red p g (Node colourS s l r))
fixGrey (Node Red p (Node colourS s l r) g) -- mirrored version of above
                            | (colour g) == Grey && (colour r) == Red = fixCase2 tree
                            | (colour g) == Grey && (colour r) == Black = fixCase3 tree
                    where
                        tree = (Node Red p (Node colourS s l r) g)
fixGrey tree = tree -- don't fix what ain't broken

fixCase1 :: RBT -> RBT
fixCase1 (Node Black p (Node Grey g leftG rightG) (Node Black s l r)) = Node Grey p (Node Black g leftG rightG) (Node Red s l r)
fixCase1 (Node Black p (Leaf Grey) (Node Black s l r)) = Node Grey p (Leaf Black) (Node Red s l r)
fixCase1 (Node Black p (Node Black s l r) (Node Grey g leftG rightG)) = Node Grey p (Node Red s l r) (Node Black g leftG rightG)
fixCase1 (Node Black p (Node Black s l r) (Leaf Grey)) = Node Grey p (Node Red s l r) (Leaf Black)

fixCase2 :: RBT -> RBT
fixCase2 (Node colourP p (Node Grey g leftG rightG) (Node Black s (Node Red l a b) r)) = Node colourP l (Node Black p (Node Black g leftG rightG) a) (Node Black s b r)
fixCase2 (Node colourP p (Leaf Grey) (Node Black s (Node Red l a b) r)) = Node colourP l (Node Black p (Leaf Black) a) (Node Black s b r)
fixCase2 (Node colourP p (Node Black s r (Node Red l a b)) (Node Grey g leftG rightG)) =  Node colourP l (Node Black s r a) (Node Black p b (Node Black g leftG rightG))
fixCase2 (Node colourP p (Node Black s r (Node Red l a b)) (Leaf Grey)) =  Node colourP l (Node Black s r a) (Node Black p b (Leaf Black))

fixCase3 :: RBT -> RBT
fixCase3 (Node _ p (Node Grey g leftG rightG) (Node Black s l r)) = Node Black s (Node Red p (Node Black g leftG rightG) l) r
fixCase3 (Node _ p (Leaf Grey) (Node Black s l r)) = Node Black s (Node Red p (Leaf Black) l) r
fixCase3 (Node _ p (Node Black s l r) (Node Grey g leftG rightG)) = Node Black s l (Node Red p r (Node Black g leftG rightG))
fixCase3 (Node _ p (Node Black s l r) (Leaf Grey)) = Node Black s l (Node Red p r (Leaf Black))

fixCase4 :: RBT -> RBT
fixCase4 (Node Black p (Node Grey g leftG rightG) (Node Black s l (Node Red r leftR rightR))) = Node Black s (Node Black p (Node Black g leftG rightG) l) (Node Black r leftR rightR)
fixCase4 (Node Black p (Leaf Grey) (Node Black s l (Node Red r leftR rightR))) = Node Black s (Node Black p (Leaf Black) l) (Node Black r leftR rightR)
fixCase4 (Node Black p (Node Black s (Node Red l leftL rightL) r) (Node Grey g leftG rightG)) = Node Black s (Node Black l leftL rightL) (Node Black p r (Node Black g leftG rightG))
fixCase4 (Node Black p (Node Black s (Node Red l leftL rightL) r) (Leaf Grey)) = Node Black s (Node Black l leftL rightL) (Node Black p r (Leaf Black))

fixCase5 :: RBT -> RBT
fixCase5 (Node Black p g (Node Red s l r)) = Node Black s (Node Red p g l) r
fixCase5 (Node Black p (Node Red s l r) g) = Node Black s l (fixGrey (Node Red p r g))

colour :: RBT -> Colour
colour (Node c _ _ _ ) = c
colour (Leaf c) = c

 ------------------------------------- Testing ------------------------------------------

testRBT = (Node Black 9 (Node Red 7 (Leaf Black) (Leaf Black)) (Node Red 15 (Leaf Black) (Leaf Black)))

test0 = showRBTree $ showRBT $ testRBT

test1Tree = balancedInsert (balancedInsert (balancedInsert (balancedInsert (balancedInsert (balancedInsert (balancedInsert (balancedInsert (balancedInsert (balancedInsert (balancedInsert testRBT 11) 10) 12) 20) 14) 3) 13) 8) 5) 16) 17
test1 = showRBTree   (showRBT
                        (test1Tree)
                    )
test2Tree = (balancedInsert (balancedInsert (balancedInsert (balancedInsert (balancedInsert (balancedInsert testRBT 11) 10) 12) 20) 14) 3)
test2 = showRBTree  (showRBT
                        (test2Tree)
                    )

testRemoveSimple = showRBTree  (showRBT
                        (removeValue test2Tree 11)
                    )
testGreyTree = removeValue test1Tree 9
testRemoveGrey = showRBTree  (showRBT
                        (testGreyTree)
                    )

testMultiple = showRBTreeList $ showRBTList [test1Tree, testGreyTree]
