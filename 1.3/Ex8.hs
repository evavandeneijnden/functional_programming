import FPPrac.Trees
import Ex4

-- data Tree4 = Leaf4
--                     | Node4 Int Tree4 Tree4
--                     deriving Show

isBalanced :: Tree4 -> Bool
isBalanced t1
                    | (treeLengthMax t1) > (treeLengthMin t1)+1 = False
                    | otherwise = True

treeLengthMax :: Tree4 -> Int
treeLengthMax Leaf4 = 0
treeLengthMax (Node4 x t1 t2)
                        | (treeLengthMax t1) > (treeLengthMax t2) = 1 + treeLengthMax t1
                        | otherwise = 1 + treeLengthMax t2

treeLengthMin :: Tree4 -> Int
treeLengthMin Leaf4 = 0
treeLengthMin (Node4 x t1 t2)
                    | (treeLengthMin t1) < (treeLengthMin t2) = 1 + treeLengthMin t1
                    | otherwise = 1 + treeLengthMin t2

splitList :: [a] -> ([a], [a])
splitList list = splitAt (((length list) + 1) `div` 2) list

balance :: Tree4 -> Tree4
balance t = balancedTree (makeList t)

balancedTree :: [Int] -> Tree4
balancedTree [] = Leaf4
balancedTree (x:xs) = Node4 x (balancedTree (fst (splitList xs))) (balancedTree (snd (splitList xs)))
