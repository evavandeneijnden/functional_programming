import FPPrac.Trees
import RBT

insert :: RBT -> Int -> RBT
insert (Node color value leftTree rightTree) new_value  | new_value < value = Node color value (insert leftTree new_value) rightTree
                                                        | otherwise = Node color value leftTree (insert rightTree new_value)
insert (Leaf colour) new_value = Node Red new_value (Leaf Black) (Leaf Black)

fixRed :: RBT -> RBT
fixRed (Node Red x (Node Red y yt1 yt2) t2) = Node Black x (Node Red y yt1 yt2) t2
fixRed (Node Red x t1 (Node Red y yt1 yt2)) = Node Black x t1 (Node Red y yt1 yt2)

-- case 2

-- case 3

-- Fixes upwards because it fixes itself and is matched in the root if the root is not matched.
-- If the root IS matched, it will fix itself and does not need to fix upwards.
fixRed (Node colour x t1 t2) | (fixed newTree) = newTree
                             | otherwise = fixRed (newTree)
                        where
                            newTree = (Node colour x (fixRed t1) (fixRed t2))

fixRed (Leaf colour) = Leaf colour
