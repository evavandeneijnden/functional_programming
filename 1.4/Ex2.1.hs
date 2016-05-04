import Ex1
import FPPrac.Trees
import Data.Char

type ParseTree = BinTree Char (Either Int Char)

parse :: String -> (ParseTree, String)
parse (x:xs)| x == '('            = ((BinNode operator l r), rest2)
            |   isDigit x   = (BinLeaf (Left $ read [x]), xs)
            |   isLetter x  = (BinLeaf (Right x), xs)
            |   otherwise   = error "kaput"
            where
                (l, (operator:rest1)) = parse xs
                (r, (')':rest2)) = parse rest1

-- Grammatica:
-- Number -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
-- Operand -> + | - | * | / | ^
-- Expression -> '(' Expression Operand Expression ')'
--            | Number
-- Start -> Expression
-- End -> Number

data State  = IntNum
            | FracNum
            | Variable
