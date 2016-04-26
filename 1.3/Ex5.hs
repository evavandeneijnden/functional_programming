import FPPrac.Trees
import Ex4

subtreeAt :: Tree4 -> Int -> Tree4
subtreeAt Leaf4 n = error "Not found"
subtreeAt (Node4 top t1 t2) n
                            | n < top = subtreeAt t1 n
                            | n > top = subtreeAt t2 n
                            | n == top = Node4 top t1 t2
