import FP_Core

codeGen :: Expr -> [Instr]
codeGen expr = instrGen expr ++ [EndProg]

instrGen :: Expr -> [Instr]
instrGen (Const x) = [Push x]
instrGen (BinExpr op expr1 expr2) = (instrGen expr1) ++ (instrGen expr2) ++ [Calc op]
