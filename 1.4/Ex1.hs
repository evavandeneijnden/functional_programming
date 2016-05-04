module BinTree where
  import FPPrac.Trees

  data BinTree a b  = BinLeaf b
                    | BinNode a (BinTree a b) (BinTree a b)

  ppBinTree :: (Show a, Show b) => (BinTree a b) -> RoseTree
  ppBinTree (BinLeaf b) = RoseNode (show b) []
  ppBinTree (BinNode a t1 t2) = RoseNode (show a) [ppBinTree t1, ppBinTree t2]

  data Unit = Nothing
            deriving Show

  type Tree1a = BinTree Int Int
  type Tree1b = BinTree (Int, Int) (Int, Int)
  type Tree1c = BinTree Unit Int
  type Tree4  = BinTree Int Unit
