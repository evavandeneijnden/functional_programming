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

        Expr    -> [[ lBracket, Expr, Op, Expr, rBracket ]
                   ,[ Nmbr                               ]
                   ,[ Var                                ]]

        Stat    -> [[ Var, ass, Expr ]]

        Repeat  -> [[ rep, Expr, Rep1 [Stat], endrep     ]]


-- shorthand names can be handy, such as:
lBracket  = Symbol "("
rBracket  = Symbol ")"

ass       = Symbol "="
rep       = Symbol "Repeat"
endrep    = Symbol "EndRep"

nmbr      = SyntCat Nmbr
op        = SyntCat Op
var       = SyntCat Var

tupleizer :: [tokenType] -> Int -> [Token]
tupleizer ((Number x):rest) i = (Nmbr,(show x),i) : (tupleizer rest i++)
tupleizer ((Text x):rest) i
                        | x == "Repeat" = (Rep,x,i) : (tupleizer rest i++)
                        | x == "EndRep" = (EndRep,x,i) : (tupleizer rest i++)
                        | otherwise = (Var,x,i) : (tupleizer rest i++)
tupleizer ((SpecialCharacter x):rest) i
                        | x elem [">",">=","<","<=","==","+","-","*"] = (Op,x,i) : (tupleizer rest i++)
                        | x == "=" = (Ass,x,i) : (tupleizer rest i++)
                        | x elem ["(",")"] = (Bracket,x,i) : (tupleizer rest i++)
                        | otherwise = Error "Unknown special character sequence"

tokenList0 = [  (Rep,"Repeat",1),
                (Nmbr,"5",2),
                (Var,"x",3),
                (Ass,"=",4),
                (Nmbr,"3",5),
                (Var,"x",6),
                (Ass,"=",7),
                (Nmbr,"6",8),
                (EndRep,"EndRep",9)
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
