import Data.Char

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

fsaOperators :: Char -> State -> State
fsaOperators character state = case state of
                            Start   | character == '*' -> Final1
                                    | character == '+' -> Final2
                                    | character == '-' -> Final3
                                    | (character == '>' || character == '<') -> Final4
                            Final1  -> Error
                            Final2  | (character == '+' || character == '=') -> Final1
                                    | otherwise -> Error
                            Final3  | (character == '-' || character == '=') -> Final1
                                    | otherwise -> Error
                            Final4  | character == '=' -> Final1
                                    | otherwise -> Error

fsaBrackets :: Char -> State -> State
fsaBrackets character state = case state of
                            Start | (character == '(' || character == ')') -> Final
                                  | otherwise -> Error
                            Final -> Error

fsaWhiteSpace :: Char -> State -> State
fsaWhiteSpace character state = case state of
                              Start | character == ' ' -> Final
                                    | otherwise -> Error
                              Final | character == ' ' -> Final
                                    | otherwise -> Error

operatorCharacters = ['*', '+', '-', '>', '<']

data Token = Num Double | Identifier String | Operator Op | LeftBracket | RightBracket
    deriving (Eq, Show)

data Op = Plus | Min | Multiply | PlusPlus | PlusIs | MinMin | MinIs | GreaterThan
        | GreaterThanEquals | SmallerThan | SmallerThanEquals
    deriving (Show, Eq)


opStrToToken :: String -> Token
opStrToToken "+" = Operator Plus
opStrToToken "-" = Operator Min
opStrToken "*" = Multiply
opStrToken "++" = PlusPlus
opStrToken "+=" = PlusIs
opStrToken "--" = MinMin
opStrToken "-=" = MinIs
opStrToken ">" = GreaterThan
opStrToken ">=" = GreaterThanEquals
opStrToken "<" = SmallerThan
opStrToken "<=" = SmallerThanEquals


numStrToToken :: String -> Token
numStrToToken i = Num (read i)


identifierToToken :: String -> Token
identifierToToken str = Identifier str

bracketStrToToken :: String -> Token
bracketStrToToken "(" = LeftBracket
bracketStrToToken ")" = RightBracket


addToken :: String -> State -> String -> (Char -> State -> State) -> (String, String)
addToken (x:xs) currentState partialToken fsa
  | nextState == Error = (partialToken, x:xs)
  | nextState == RealError = error "Parse error!"
  | otherwise          = addToken xs nextState (partialToken ++ [x]) fsa
    where
      nextState = fsa x currentState

replace :: String -> String -> [Char] -> [Char]
replace _ _ [] = []
replace toReplace replacement str  | (take lenRepl str) == toReplace = replacement ++ (replace toReplace replacement (drop lenRepl str))
                                   | otherwise = (take 1 str) ++ (replace toReplace replacement (drop 1 str))
                                   where
                                     lenRepl = length toReplace

tokenizer':: String -> [Token]
-- Removes whitespace from string before tokenizing it
tokenizer' input = tokenizer (replace " " "" input)

tokenizer :: String -> [Token]
tokenizer [] = []
tokenizer (x:xs)  = (toToken partialTokenStr) : (tokenizer rest)
                  where
                      (fsa, toToken) = chooseFsa x
                      (partialTokenStr, rest) = addToken (x:xs) Start "" fsa


chooseFsa :: Char -> (FSA, String -> Token)
chooseFsa x | (isDigit x)                 = (fsaNumbers, numStrToToken)
            | (isAlpha x)                 = (fsaIdentifiers, identifierToToken)
            | (elem x operatorCharacters) = (fsaOperators, opStrToToken)
            | (x == '(' || x == ')')      = (fsaWhiteSpace, bracketStrToToken)
            | otherwise = error "Invalid input"
