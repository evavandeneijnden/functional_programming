module Ex4 where
            import Ex1
            import Ex2
            import Ex3

            type ParseTreeTokens = BinTree Token Token

            parseToken :: [Token] -> (ParseTreeTokens, [Token])
            parseToken (x:xs)       | x == LeftBracket                  = ((BinNode operator l r), rest2)
                                    | x == Num _ || x == Identifier _   = (BinLeaf x, xs)
                                    | otherwise                         = error "kaput"
                                    where
                                                (l, (operator:rest1)) = parseToken xs
                                                (r, (LeftBracket:rest2)) = parseToken rest1
