import FPPrac.Trees
import RBT

insert :: RBT -> Int -> RBT
insert (Node color value (Leaf _) (Leaf _)) new_value   | new_value < value = (Node color value new_node (Leaf Black))
                                                        | otherwise = (Node color value (Leaf Black) new_node)
                                                        where
                                                          new_node = (Node Red new_value (Leaf Black) (Leaf Black))
insert (Node color value leftTree rightTree) new_value  | new_value < value = Node color value (insert leftTree new_value) rightTree
                                                        | otherwise = Node color value leftTree (insert rightTree new_value)
