module Ex4 where
            import FPPrac.Trees
            import Ex1

            data Tree4 = Leaf4
                        | Node4 Int Tree4 Tree4
                        deriving Show

            t4 = Node4 10 (Node4 5 (Node4 4 Leaf4 Leaf4) (Node4 6 Leaf4 Leaf4)) (Node4 11 Leaf4 Leaf4)

            ppt4 :: Tree4 -> RoseTree
            ppt4 Leaf4 = RoseNode "" []
            ppt4 (Node4 top t1 t2) = RoseNode (show top) [ppt4 t1, ppt4 t2]

            insertTree :: Tree4 -> Int -> Tree4
            insertTree Leaf4 x = Node4 x Leaf4 Leaf4
            insertTree (Node4 top t1 t2) x
                                            | x < top = Node4 top (insertTree t1 x) t2
                                            | otherwise = Node4 top t1 (insertTree t2 x)

            t5 = insertTree t4 7

            makeTree :: [Int] -> Tree4
            makeTree list = makeTree2 list Leaf4

            makeTree2 :: [Int] -> Tree4 -> Tree4
            makeTree2 [] t4 = t4
            makeTree2 (x:xs) t4 = makeTree2 xs (insertTree t4 x)

            makeTreeFoldl :: [Int] -> Tree4
            makeTreeFoldl list = foldl insertTree Leaf4 list

            list = [1::Int,2,3,4,5,6,7,8,9,0]

            makeList :: Tree4 -> [Int]
            makeList (Leaf4) = []
            makeList (Node4 x t1 t2) = (makeList t1) ++ [x] ++ (makeList t2)

            listSort :: [Int] -> [Int]
            listSort list = makeList (makeTree list)

            treeSort :: Tree4 -> Tree4
            treeSort t = makeTree (makeList t)
