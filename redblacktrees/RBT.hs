module RBT where
    import FPPrac.Trees

    -- Node Colour Value LeftSubTree RightSubTree
    data RBT    = Node Colour Int RBT RBT
                | Leaf Colour
            deriving Eq

    data Colour = Red
                | Black
                | Grey
            deriving Eq

    showRBT :: RBT -> RBTree
    showRBT (Node Red x t1 t2) = RBNode NodeRed (show x) [(showRBT t1), (showRBT t2)]
    showRBT (Node Black x t1 t2) = RBNode NodeBlack (show x) [(showRBT t1), (showRBT t2)]
    showRBT (Node Grey x t1 t2) = RBNode NodeGrey (show x) [(showRBT t1), (showRBT t2)]
    showRBT (Leaf Red) = RBNode NodeRed "" []
    showRBT (Leaf Black) = RBNode NodeBlack "" []
