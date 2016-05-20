{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}
        -- Necessary for function toRoseTree

module Grrr where

{- ===========================================================================
Contains example grammar + examples of test definitions
NOTE: Compiler directives above
=========================================================================== -}

import FPPrac.Trees       -- Contains now also the function toRoseTree. Re-install it!
import GHC.Generics       -- Necessary for correct function of FPPrac

import FP_TypesEtc           -- Extend the file TypesEtc with your own alphabet
import FP_ParserGen (parse)  -- Touching this file leaves you at your own devices
-- import Tokenizer       -- You'll have to write a file for tokenizing yourself

grrr :: Grammar

grrr nt = case nt of

        Nmbr    -> [[ nmbr                               ]]

        Op      -> [[ op                                 ]]

        Var     -> [[ var                                ]]

        Ass     -> [[ ass                                ]]

        Key     -> [[ key ]]

        Expr    -> [[ lBracket, Expr, Op, Expr, rBracket ]
                   ,[ Nmbr                               ]
                   ,[ Var                                ]
                   ,[ termIf, Expr, termThen, Expr, termElse, Expr    ]]

        Stat    -> [[ Var, ass, Expr ],
                    [ Repeat ]]

        Repeat  -> [[ termRepeat, Expr, Rep1 [Stat], termEndRep        ]]


-- shorthand names can be handy, such as:
lBracket  = Symbol "("
rBracket  = Symbol ")"

ass       = Symbol "="
key       = SyntCat Key

termRepeat  = Terminal "Repeat"
termEndRep  = Terminal "EndRep"
termIf      = Terminal "If"
termThen    = Terminal "Then"
termElse    = Terminal "Else"

nmbr      = SyntCat Nmbr
op        = SyntCat Op
var       = SyntCat Var

tupleizer :: [TokenType] -> Int -> [Token]
tupleizer [] i = []
tupleizer ((Number x):rest) i = (Nmbr,(show x),i) : (tupleizer rest (i+1))
tupleizer ((Text x):rest) i
                        | elem x ["Repeat","EndRep","If","Then","Else"] = (Key,x,i) : (tupleizer rest (i+1))
                        | otherwise = (Var,x,i) : (tupleizer rest (i+1))
tupleizer ((SpecialCharacter x):rest) i
                        | elem x [">",">=","<","<=","==","+","-","*"] = (Op,x,i) : (tupleizer rest (i+1))
                        | x == "=" = (Ass,x,i) : (tupleizer rest (i+1))
                        | elem x ["(",")"] = (Bracket,x,i) : (tupleizer rest (i+1))
                        | otherwise = error "Unknown special character sequence"

-- data ParseTree  = PLeaf Token
--                 | PNode Alphabet [ParseTree]
--                 | PError ParseTree [Alphabet] Alphabet String Int
--                 deriving (Eq,Show,Generic,ToRoseTree)

data Opr    = Add | Mul | Sub
        deriving (Eq,Ord,Show,Generic,ToRoseTree)

data Ex     = Const Double                   -- for constants
          | Variable String
          | BinEx Opr Ex Ex        -- for ``binary expressions''
          | IfEx Ex Ex Ex
        deriving (Eq,Ord,Show,Generic,ToRoseTree)

data Statement = Assign String Ex
              |  Rpt Ex [Statement]
        deriving (Eq,Ord,Show,Generic,ToRoseTree)

cleanTreeS :: ParseTree -> Statement
cleanTreeS (PNode Repeat (startrep:expr:rest)) = Rpt (cleanTreeE expr) (map cleanTreeS (init rest))
cleanTreeS (PNode Stat ((PNode Var var):exprList)) = Assign (cleanTreeL (head var)) (cleanTreeE (head exprList))

cleanTreeE (PNode Expr ((PNode Expr expr1):op:rest)) = BinEx (cleanTreeO op) (cleanTreeE (PNode Expr expr1)) (cleanTreeE (head rest))
cleanTreeE (PNode Expr ((PNode Nmbr nr):rest)) = Const (read (cleanTreeL (head nr)))
cleanTreeE (PNode Expr ((PNode Var var):rest)) = Variable (cleanTreeL (head var))
cleanTreeE (PNode Expr ((PLeaf _):rest)) = IfEx (cleanTreeE (head rest)) (cleanTreeE (rest !! 3)) (cleanTreeE (rest !! 5))

cleanTreeO (PNode Op op)
                    | cleanTreeL (head op) == "*" = Mul
                    | cleanTreeL (head op) == "+" = Add
                    | cleanTreeL (head op) == "-" = Sub

cleanTreeL (PLeaf (_,x,_)) = x

tokenList0 = [  (Key,"Repeat",1),
                (Nmbr,"5",2),
                (Var,"x",3),
                (Ass,"=",4),
                (Nmbr,"3",5),
                (Var,"x",6),
                (Ass,"=",7),
                (Nmbr,"6",8),
                (Key,"EndRep",9)
             ]
            --  [ (Bracket,"(",1)
            --  , (Nmbr,"10",2)
            --  , (Op,"+",3)
            --  , (Nmbr,"20",4)
            --  , (Bracket,")",5)
            --  , (Op,"*",6)
            --  , (Nmbr,"30",7)
            --  , (Bracket,")",8)
            --  ]

parseTree0 = parse grrr Repeat tokenList0

-- prpr: for pretty-printing the parsetree, including error messages
testTxt    = prpr parseTree0

-- showTree + toRoseTree: for graphical representation in browser
testGr     = showTree $ toRoseTree parseTree0
