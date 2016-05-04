-- Grammatica:
-- Number -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
-- Operand -> + | - | * | / | ^
-- Expression -> '(' Expression Operand Expression ')'
--            | Number
import BinTree
import Data.Char

data Token  = Num Int
            | Op Char
            | LeftBracket
            | RightBracket

isOperand :: Char -> Bool
isOperand x =  elem x ['+','-','*','/','^']


tokenize :: String -> [Token]
tokenize [] = []
tokenize (x:xs) |   x == '('       = LeftBracket : (tokenize xs)
                |   x == ')'       = RightBracket : (tokenize xs)
                |   isDigit x      = (Num (read [x])) : (tokenize xs)
                |   isOperand x    = (Op x) : (tokenize xs)
                |   otherwise      = error "Invalid token found"

type ParseTree = BinTree Char Char

parseExpression :: String -> (ParseTree)
parseExpression string  = rest == ""        = parsetree
                        | otherwise         = error "malformed input"
                        where
                          tokens            = (tokenize string)
                          (parsetree, rest) = parse Expression tokens

data Nonterminal = Expression
                  | Operand
                  | Number

parse :: Nonterminal -> [Token] -> (ParseTree, String)
parse Expression (x:xs) = x == LeftBracket  = ((BinNode (head xs) t1 t2), r2)
                        | x == (Num y)      = parse Number (x:xs)
                        | otherwise         = (error "parse error")
                        where
                          (t1, r1)          = parse Expression (init xs)
                          (t2, r2)          = parse Expression r1

parse Number (x:xs) = x == (Num y)          = ((BinLeaf y), xs)
                    | otherwise             = (error "parse error")
