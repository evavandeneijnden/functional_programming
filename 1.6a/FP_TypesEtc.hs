{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}

{- ===========================================================================
Contains basic types - you'll have to extend several of the definitions below
=========================================================================== -}


module FP_TypesEtc where

import GHC.Generics
import FPPrac.Trees
import Data.Char

-- ===================================================================
-- Example Alphabet
-- - Extend, adapt, change the non-terminals to your own needs
-- - Do NOT change the first two groups of constructors (Symbol ... Rep1)

data Alphabet = Terminal String               -- Terminal symbol: WILL be included in parseTree
              | Symbol   String               -- Terminal symbol: will NOT be included in parseTree
              | SyntCat  Alphabet             -- Checks whether a string belongs to a syntactic category

              | Alt   [Alphabet] [Alphabet]   -- Try both
              | Opt   [Alphabet]              -- Optional
              | Rep0  [Alphabet]              -- Zero or more repetitions
              | Rep1  [Alphabet]              -- One or more repetitions

              | Expr                          -- Expression
              | Nmbr                          -- Number
              | Var                           -- Variables
              | Op                            -- Operation symbol
              | Space                         -- Spaces
              | Bracket                       -- Brackets

              | Stat                          -- Statement
              | Ass                           -- Assignment
              | Repeat                        -- Repeat
              | Key                           -- Keyword
              deriving (Eq,Ord,Show,Generic,ToRoseTree)

-- ===================================================================
-- Symbolic notation for EBNF constructors

ps <> qs = Alt  ps qs
(?:) ps  = Opt  ps
(*:) ps  = Rep0 ps
(+:) ps  = Rep1 ps

-- ===================================================================

type Grammar = Alphabet -> [[Alphabet]]

type Token   = (Alphabet,String,Int)  -- Alphabet: indicates the "syntactic category" to which
                                      --      the String belongs (to distinguish, a.o., between
                                      --      reserved words and identifiers in general),
                                      -- String: the token itself,
                                      -- Int: the position of the token in the input token-list
                                      --      (needed for error messages).



data TokenType = Number Double
                | Text String
                | SpecialCharacter String
    deriving (Show)

data State = Start | State1 | State2 | Final | Final1 | Final2 | Final3 | Final4 | Error | RealError
  deriving (Show, Eq)

type FSA = Char -> State -> State

fsaNumbers :: Char -> State -> State
fsaNumbers character state = case state of
                            Start  | (isDigit character) -> State1
                            State1 | (isDigit character) -> State1
                                   | character == '.' -> State2
                                   | otherwise -> Error
                            State2        | (isDigit character) -> Final
                                          | otherwise ->RealError
                            Final         | (isDigit character) -> Final
                                          | otherwise -> Error

fsaIdentifiers :: Char -> State -> State
fsaIdentifiers character state = case state of
                              Start | (isAlpha character) -> Final
                                    | otherwise -> Error
                              Final | (isAlphaNum character) -> Final
                                    | otherwise -> Error

fsaSpecialCharacters :: Char -> State -> State
fsaSpecialCharacters character state = case state of
                            Start   | character == '*' -> Final1
                                    | character == '+' -> Final1
                                    | character == '-' -> Final1
                                    | (character == '>' || character == '<') -> Final4
                                    | (character == '=') -> Final4
                                    | (character == '(') -> Final1
                                    | (character == ')') -> Final1
                            Final1  -> Error
                            Final4  | character == '=' -> Final1
                                    | otherwise -> Error

charStrToToken :: String -> TokenType
charStrToToken str = SpecialCharacter str


numStrToToken :: String -> TokenType
numStrToToken i = Number (read i)


identifierToToken :: String -> TokenType
identifierToToken str = Text str





specialCharacters = ['*', '+', '-', '>', '<', '(', ')', '=']

chooseFsa :: Char -> (FSA, String -> TokenType)
chooseFsa x | (isDigit x)                 = (fsaNumbers, numStrToToken)
            | (isAlpha x)                 = (fsaIdentifiers, identifierToToken)
            | (elem x specialCharacters) = (fsaSpecialCharacters, charStrToToken)
            | otherwise = error ("Invalid input "++[x])

addTokenType :: String -> State -> String -> (Char -> State -> State) -> (String, String)
addTokenType [] _ partialToken _ = (partialToken,"")
addTokenType (x:xs) currentState partialToken fsa
  | nextState == Error = (partialToken, (x:xs))
  | nextState == RealError = error "Parse error!"
  | otherwise          = addTokenType xs nextState (partialToken ++ [x]) fsa
    where
      nextState = fsa x currentState

replace :: String -> String -> [Char] -> [Char]
replace _ _ [] = []
replace toReplace replacement str  | (take lenRepl str) == toReplace = replacement ++ (replace toReplace replacement (drop lenRepl str))
                                   | otherwise = (take 1 str) ++ (replace toReplace replacement (drop 1 str))
                                   where
                                     lenRepl = length toReplace

tokenizer':: String -> [TokenType]
-- Removes whitespace from string before tokenizing it
tokenizer' input = tokenizer (replace " " "" input)

tokenizer :: String -> [TokenType]
tokenizer [] = []
tokenizer (' ':xs) = tokenizer xs
tokenizer (x:xs)  = (toToken partialTokenStr) : (tokenizer rest)
                  where
                      (fsa, toToken) = chooseFsa x
                      (partialTokenStr, rest) = addTokenType (x:xs) Start "" fsa





instance ToRoseTree Token where
  toRoseTree t = RoseNode (show t) []

data ParseTree  = PLeaf Token
                | PNode Alphabet [ParseTree]
                | PError ParseTree [Alphabet] Alphabet String Int
                deriving (Eq,Show,Generic,ToRoseTree)

instance Ord ParseTree where
  PError _ _ _ _ k <  PError _ _ _ _ k' = k <  k'
  _                <  _                 = error "ordering only in case of parse-errors"

  PError _ _ _ _ k <= PError _ _ _ _ k' = k <= k'
  _                <= _                 = error "ordering only in case of parse-errors"

type ParseState = ( Alphabet       -- Non-terminal indicating the present subexpression
                  , [ParseTree]    -- The already produced trees within the present subexpression
                  , [Token]        -- The remaining list of input tokens
                  )

-- ===================================================================
x ∈ xs = x `elem` xs

-- ===================================================================
-- Pretty Printing

toStrings tree = case tree of
     PLeaf t                 -> ["PLeaf " ++ show t]

     PNode nt ts             -> ("PNode " ++ show nt) : (addSpace 7 $ concat $ addEndBrack $ addListNotation $ map toStrings ts)
                             where
                               addSpace n = map ((replicate n ' ') ++)

                               addListNotation ((str:strs):strss) =   (("["++str):strs)
                                                                    : [  (","++str'):strs' | (str':strs') <- strss ]

                               addEndBrack [strs]       = [ strs ++ ["]"] ]
                               addEndBrack (strs:strss) = strs : addEndBrack strss

     PError tr rule nt str k -> [ "==========="
                                , "Parse Error"
                                , "==========="
                                , "Recognized:"
                                , "-----------"
                                ]
                                ++ toStrings tr ++
                                [ "-----------"
                                , "Still to go:   " ++ show rule
                                , "Expected:      " ++ show nt
                                , "Found:         " ++ str
                                , "At position:   " ++ show k
                                , "==========="
                                ]

prpr t  = putStr $ ('\n':) $ (++"\n") $ unlines $ toStrings t
