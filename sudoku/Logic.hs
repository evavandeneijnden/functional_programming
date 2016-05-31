type Sudoku = [Cell]
data Cell = Cell  { rownum :: Int,
                    colnum :: Int,
                    blocknum :: Int,
                    value :: Int }
          deriving Show


rowpeers :: Cell -> Sudoku -> [Cell]
rowpeers cell sudoku = rowpeers' cell sudoku []

rowpeers' :: Cell -> Sudoku -> [Cell] -> [Cell]
rowpeers' _ [] partial_res = partial_res
rowpeers' cell (c:cs) partial_res | (rownum c) == (rownum cell) = (rowpeers' cell cs (c : partial_res))
                                  | otherwise = (rowpeers' cell cs partial_res)



-- Building example sudoku
c00 = Cell{rownum  = 0, colnum = 0, blocknum = 0, value = 8}
c01 = Cell{rownum  = 0, colnum = 1, blocknum = 0, value = 1}
c02 = Cell{rownum  = 0, colnum = 2, blocknum = 0, value = -1}
c03 = Cell{rownum  = 0, colnum = 3, blocknum = 1, value = -1}
c04 = Cell{rownum  = 0, colnum = 4, blocknum = 1, value = -1}
c05 = Cell{rownum  = 0, colnum = 5, blocknum = 1, value = -1}
c06 = Cell{rownum  = 0, colnum = 6, blocknum = 2, value = 7}
c07 = Cell{rownum  = 0, colnum = 7, blocknum = 2, value = -1}
c08 = Cell{rownum  = 0, colnum = 8, blocknum = 2, value = 3}

c10 = Cell{rownum  = 1, colnum = 0, blocknum = 0, value = -1}
c11 = Cell{rownum  = 1, colnum = 1, blocknum = 0, value = -1}
c12 = Cell{rownum  = 1, colnum = 2, blocknum = 0, value = -1}
c13 = Cell{rownum  = 1, colnum = 3, blocknum = 1, value = 6}
c14 = Cell{rownum  = 1, colnum = 4, blocknum = 1, value = -1}
c15 = Cell{rownum  = 1, colnum = 5, blocknum = 1, value = 7}
c16 = Cell{rownum  = 1, colnum = 6, blocknum = 2, value = -1}
c17 = Cell{rownum  = 1, colnum = 7, blocknum = 2, value = -1}
c18 = Cell{rownum  = 1, colnum = 8, blocknum = 2, value = 8}

c20 = Cell{rownum  = 2, colnum = 0, blocknum = 0, value = 9}
c21 = Cell{rownum  = 2, colnum = 1, blocknum = 0, value = -1}
c22 = Cell{rownum  = 2, colnum = 2, blocknum = 0, value = 2}
c23 = Cell{rownum  = 2, colnum = 3, blocknum = 1, value = 3}
c24 = Cell{rownum  = 2, colnum = 4, blocknum = 1, value = 1}
c25 = Cell{rownum  = 2, colnum = 5, blocknum = 1, value = -1}
c26 = Cell{rownum  = 2, colnum = 6, blocknum = 2, value = 6}
c27 = Cell{rownum  = 2, colnum = 7, blocknum = 2, value = -1}
c28 = Cell{rownum  = 2, colnum = 8, blocknum = 2, value = -1}

c30 = Cell{rownum = 3, colnum = 0, blocknum = 3, value = -1}
c31 = Cell{rownum = 3, colnum = 1, blocknum = 3, value = 4}
c32 = Cell{rownum = 3, colnum = 2, blocknum = 3, value = -1}
c33 = Cell{rownum = 3, colnum = 3, blocknum = 4, value = -1}
c34 = Cell{rownum = 3, colnum = 4, blocknum = 4, value = 7}
c35 = Cell{rownum = 3, colnum = 5, blocknum = 4, value = -1}
c36 = Cell{rownum = 3, colnum = 6, blocknum = 5, value = 5}
c37 = Cell{rownum = 3, colnum = 7, blocknum = 5, value = 6}
c38 = Cell{rownum = 3, colnum = 8, blocknum = 5, value = -1}

c40 = Cell{rownum = 4, colnum = 0, blocknum = 3, value = -1}
c41 = Cell{rownum = 4, colnum = 1, blocknum = 3, value = -1}
c42 = Cell{rownum = 4, colnum = 2, blocknum = 3, value = 7}
c43 = Cell{rownum = 4, colnum = 3, blocknum = 4, value = 9}
c44 = Cell{rownum = 4, colnum = 4, blocknum = 4, value = -1}
c45 = Cell{rownum = 4, colnum = 5, blocknum = 4, value = 1}
c46 = Cell{rownum = 4, colnum = 6, blocknum = 5, value = 2}
c47 = Cell{rownum = 4, colnum = 7, blocknum = 5, value = -1}
c48 = Cell{rownum = 4, colnum = 8, blocknum = 5, value = -1}

c50 = Cell{rownum = 5, colnum = 0, blocknum = 3, value = -1}
c51 = Cell{rownum = 5, colnum = 1, blocknum = 3, value = 6}
c52 = Cell{rownum = 5, colnum = 2, blocknum = 3, value = 3}
c53 = Cell{rownum = 5, colnum = 3, blocknum = 4, value = -1}
c54 = Cell{rownum = 5, colnum = 4, blocknum = 4, value = 4}
c55 = Cell{rownum = 5, colnum = 5, blocknum = 4, value = -1}
c56 = Cell{rownum = 5, colnum = 6, blocknum = 5, value = -1}
c57 = Cell{rownum = 5, colnum = 7, blocknum = 5, value = 9}
c58 = Cell{rownum = 5, colnum = 8, blocknum = 5, value = -1}

c60 = Cell{rownum = 6, colnum = 0, blocknum = 6, value = -1}
c61 = Cell{rownum = 6, colnum = 1, blocknum = 6, value = -1}
c62 = Cell{rownum = 6, colnum = 2, blocknum = 6, value = 4}
c63 = Cell{rownum = 6, colnum = 3, blocknum = 7, value = -1}
c64 = Cell{rownum = 6, colnum = 4, blocknum = 7, value = 9}
c65 = Cell{rownum = 6, colnum = 5, blocknum = 7, value = 2}
c66 = Cell{rownum = 6, colnum = 6, blocknum = 8, value = 1}
c67 = Cell{rownum = 6, colnum = 7, blocknum = 8, value = -1}
c68 = Cell{rownum = 6, colnum = 8, blocknum = 8, value = 6}

c70 = Cell{rownum = 7, colnum = 0, blocknum = 6, value = 6}
c71 = Cell{rownum = 7, colnum = 1, blocknum = 6, value = -1}
c72 = Cell{rownum = 7, colnum = 2, blocknum = 6, value = -1}
c73 = Cell{rownum = 7, colnum = 3, blocknum = 7, value = 5}
c74 = Cell{rownum = 7, colnum = 4, blocknum = 7, value = -1}
c75 = Cell{rownum = 7, colnum = 5, blocknum = 7, value = 4}
c76 = Cell{rownum = 7, colnum = 6, blocknum = 8, value = -1}
c77 = Cell{rownum = 7, colnum = 7, blocknum = 8, value = -1}
c78 = Cell{rownum = 7, colnum = 8, blocknum = 8, value = -1}

c80 = Cell{rownum = 8, colnum = 0, blocknum = 6, value = 7}
c81 = Cell{rownum = 8, colnum = 1, blocknum = 6, value = -1}
c82 = Cell{rownum = 8, colnum = 2, blocknum = 6, value = 8}
c83 = Cell{rownum = 8, colnum = 3, blocknum = 7, value = -1}
c84 = Cell{rownum = 8, colnum = 4, blocknum = 7, value = -1}
c85 = Cell{rownum = 8, colnum = 5, blocknum = 7, value = -1}
c86 = Cell{rownum = 8, colnum = 6, blocknum = 8, value = -1}
c87 = Cell{rownum = 8, colnum = 7, blocknum = 8, value = 5}
c88 = Cell{rownum = 8, colnum = 8, blocknum = 8, value = 9}

testSudoku = [c00, c01, c02, c03, c04, c05, c06, c07, c08, c10, c11, c12, c13, c14, c15, c16, c17, c18, c20, c21, c22, c23, c24, c25, c26, c27, c28, c30, c31, c32, c33, c34, c35, c36, c37, c38, c40, c41, c42, c43, c44, c45, c46, c47, c48, c50, c51, c52, c53, c54, c55, c56, c57, c58, c60, c61, c62, c63, c64, c65, c66, c67, c68, c70, c71, c72, c73, c74, c75, c76, c77, c78 ]