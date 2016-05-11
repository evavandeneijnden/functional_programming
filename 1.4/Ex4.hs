module Ex4 where
            import Ex2
            import Ex3

            type ParseTreeToken = BinTree Operator (Either Num Identifier)

            parseToken :: [Token] -> (ParseTreeToken, [Token])
            parseToken (x:xs)| x == LeftBracket      = ((BinNode operator l r), rest2)
                        | x == Num _            = (BinLeaf (Left x), xs)
                        | x == Identifier _     = (BinLeaf (Right x), xs)
                        | otherwise             = error "kaput"
                        where
                            (l, (operator:rest1)) = parseToken xs
                            (r, (')':rest2)) = parseToken rest1
