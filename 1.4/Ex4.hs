import Ex2
import Ex3
import FPPrac.Trees
import Data.Char

type ParseTreeToken = BinTree Operator (Either Num Identifier)

parse :: [Token] -> (ParseTree, [Token])
parse (x:xs)| x == LeftBracket      = ((BinNode operator l r), rest2)
            |   isNumToken x        = (BinLeaf (Left x), xs)
            |   isIdentifierToken x = (BinLeaf (Right x), xs)
            |   otherwise   = error "kaput"
            where
                (l, (operator:rest1)) = parse xs
                (r, (')':rest2)) = parse rest1
