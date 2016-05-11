import Ex1
import Ex4
import Ex3
import FPPrac.Trees

eval :: [(String,Double)] -> String -> Double
eval values expression = calc values (fst (parseToken (tokenizer' expression)))

calc :: [(String,Double)] -> ParseTreeTokens -> Double
calc values (BinNode Plus t1 t2) = (calc values t1) + (calc values t2)
calc values (BinNode Min t1 t2) = (calc values t1) - (calc values t2)
calc values (BinNode Multiply t1 t2) = (calc values t1) * (calc values t2)
calc values (BinNode GreaterThan t1 t2)
                | (calc values t1) > (calc values t2) = 1.0
                | otherwise = 0.0
calc values (BinNode GreaterThanEquals t1 t2)
                | (calc values t1) >= (calc values t2) = 1.0
                | otherwise = 0.0
calc values (BinNode SmallerThan t1 t2)
                | (calc values t1) < (calc values t2) = 1.0
                | otherwise = 0.0
calc values (BinNode SmallerThanEquals t1 t2)
                | (calc values t1) <= (calc values t2) = 1.0
                | otherwise = 0.0

calc values (BinLeaf (Left x)) = getValue values x

calc values (BinLeaf (Right x)) = x

getValue :: [(String,Double)] -> String -> Double
getValue [] var = error "Variable not found"
getValue (x:xs) var
                | fst x == var = snd x
                | otherwise = getValue xs var
