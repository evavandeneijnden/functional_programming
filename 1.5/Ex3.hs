import FP_Core
import FPPrac.Trees

data BinTree a b  = BinLeaf b
                | BinNode a (BinTree a b) (BinTree a b)

ppBinTree :: (Show a, Show b) => (BinTree a b) -> RoseTree
ppBinTree (BinLeaf b) = RoseNode (show b) []
ppBinTree (BinNode a t1 t2) = RoseNode (show a) [ppBinTree t1, ppBinTree t2]

type ExprTree = BinTree Op Int

exprToExprTree :: Expr -> ExprTree
exprToExprTree (Const x) = BinLeaf x
exprToExprTree (BinExpr op expr1 expr2) = BinNode op (exprToExprTree expr1) (exprToExprTree expr2)
