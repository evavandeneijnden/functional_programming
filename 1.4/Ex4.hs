module Ex4 where
            import Ex1
            import Ex2
            import Ex3
            import FPPrac.Trees

            type ParseTreeTokens = BinTree Op (Either String Double)

            parseToken :: [Token] -> (ParseTreeTokens, [Token])
            parseToken [] = error "kaput"
            parseToken (x:xs) = case x of
                                    LeftBracket -> ((BinNode operator l r), rest2)
                                    Num y -> (BinLeaf (Right y), xs)
                                    Identifier y -> (BinLeaf (Left y), xs)
                                    _ -> error "Unknown token"
                        where
                                    (l, (token:rest1)) = parseToken xs
                                    operator = getOp token
                                    (r, (RightBracket:rest2)) = parseToken rest1

            getOp :: Token -> Op
            getOp (Operator op) = op
