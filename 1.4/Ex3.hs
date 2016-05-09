import Data.Char

data State = Start | StartandFinal | State1 | Final | Final1 | Final2 | Final3 | Final4 | Error

fsaNumbers :: char -> State -> State
fsaNumbers character state = case state of
                            StartAndFinal | (isDigit character) = StartandFinal
                                          | character == '.' = State1
                                          | otherwise = Error
                            State1        | (isDigit character) = Final
                                          | otherwise = Error
                            Final         | (isDigit character) = Final
                                          | otherwise = Error

fsaIdentifiers :: char -> State -> State
fsaIdentifiers character state = case state of
                              Start | (isAlpha character) = Final
                                    | otherwise = Error
                              Final | (isAlphaNum character) = Final
                                    | otherwise = Error

fsaOperators :: char -> State -> State
fsaOperators character state = case state of
                            Start   | character == '*' = Final1
                                    | character == '+' = Final2
                                    | character == '-' = Final3
                                    | (character == '>' || character == '<') = Final4
                            Final1  = Error
                            Final2  | (character == '+' || character == '=') = Final1
                                    | otherwise = Error
                            Final3  | (character == '-' || character == '=') = Final1
                                    | otherwise = Error
                            Final4  | character == '=' = Final1
                                    | otherwise = Error

fsaBrackets :: char -> State -> State
fsaBrackets character state = case state of
                            Start | (character == '(' || character == ')') = Final
                                  | otherwise = Error
                            Final = Error

fsaWhiteSpace :: char -> State -> State
fsaWhiteSpace character state = case state of
                              Start | character = ' ' = Final
                                    | otherwise = Error
                              Final | character = ' ' = Final
                                    | otherwise = Error

operatorCharacters = ['*', '+', '-', '>', '<']


tokenizer :: String -> [Token]
tokenizer [x:xs]  = case x of
                  (isDigit x)                 | fsaNumbers
                  (isAlpha x)                 | fsaIdentfifiers
                  (elem operatorCharacters x) | fsaOperators
