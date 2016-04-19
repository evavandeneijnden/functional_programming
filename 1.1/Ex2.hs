import Data.Char
code :: Char -> Int -> Char
code letter inc     | 97 <= ord letter && ord letter <= 122 && (ord letter + inc) > 122 = chr (ord letter + inc - 26)
                    | 97 <= ord letter && ord letter <= 122 = chr (ord letter + inc)
                    | 65 <= ord letter && ord letter <= 90 && (ord letter + inc) > 90 = chr (ord letter + inc -26)
                    | 65 <= ord letter && ord letter <= 90 = chr (ord letter + inc)
