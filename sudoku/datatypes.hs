type Sudoku = [Cell]
data Cell = Cell  { rownum :: Int,
                    colnum :: Int,
                    blocknum :: Int,
                    value :: Int }

rowpeers :: Cell -> Sudoku -> [Cell]
rowpeers cell sudoku = rowpeers' cell sudoku []

rowpeers' :: Cell -> Sudoku -> [Cell] -> [Cell]
rowpeers' _ [] partial_res = partial_res
rowpeers' cell (c:cs) partial_res = (rownum c) == (rownum cell) = (rowpeers' cell cs (c : partial_res))
                                  | otherwise = (rowpeers' cell cs partial_res)
