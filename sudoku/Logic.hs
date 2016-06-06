module Logic where
  import Data.List
  import Data.Maybe

  type Sudoku = [[Cell]]

  data Cell = Cell {value :: Int, coords :: (Int, Int), block :: Int}
-- coords = (rownum, colnum)
            deriving (Show, Eq)

  blockPeers :: Cell -> Sudoku -> [Cell]
  blockPeers cell sudoku = blockPeers' cell sudoku []

  blockPeers' :: Cell -> Sudoku -> [Cell] -> [Cell]
  blockPeers' cell (row:rows) partial_result = blockPeers' cell rows (partial_result ++ (rowBlockPeers cell row))

  rowBlockPeers ::Cell -> [Cell] -> [Cell]
  rowBlockPeers cell row = rowBlockPeers' cell row []

  rowBlockPeers' :: Cell -> [Cell] -> [Cell] -> [Cell]
  rowBlockPeers' cell [] partial_result = partial_result
  rowBlockPeers' cell (c:cells) partial_result  | (block cell) == (block c) = rowBlockPeers' cell cells (partial_result ++ [c])
                                                | otherwise = rowBlockPeers' cell cells partial_result

  rowPeers :: Cell -> Sudoku -> [Cell]
  rowPeers cell sudoku = ys ++ (tail zs)
                      where
                        rownum = fst (coords cell)
                        colnum = snd (coords cell)
                        complete_row = sudoku !! rownum
                        (ys, zs) = splitAt colnum complete_row

  colPeers :: Cell -> Sudoku -> [Cell]
  colPeers cell sudoku = colPeers' rowNo colNo  sudoku []
                            where
                              (rowNo, colNo) = coords cell

  colPeers' :: Int -> Int -> Sudoku -> [Cell] -> [Cell]
  colPeers' _ _ [] partial_result = partial_result
  colPeers' rowNo colNo (row:rows) partial_result | rowNo == currentRowNo = colPeers' rowNo colNo rows partial_result
                                                  | otherwise = colPeers' rowNo colNo rows (partial_result ++ [row !! colNo])
                                                  where
                                                    currentRowNo = fst (coords (head row))


  possibleValues :: Cell -> Sudoku -> [Int]
  possibleValues cell sudoku = optionsAfterColCompare
                      where
                        allOptions = [1..9]
                        blockValues = (map value (blockPeers cell sudoku))
                        rowValues = (map value (rowPeers cell sudoku))
                        colValues = (map value (colPeers cell sudoku))
                        optionsAfterBlockCompare = (filter (\a -> notElem a blockValues) allOptions)
                        optionsAfterRowCompare = (filter (\a -> notElem  a rowValues) optionsAfterBlockCompare)
                        optionsAfterColCompare = (filter (\a -> notElem a colValues) optionsAfterRowCompare)

  firstEmptyCell :: Sudoku -> Int -> Cell
  firstEmptyCell (r:rows) noToSkip  =  case maybeCell of
                                    Just cell -> cell
                                    Nothing -> firstEmptyCell rows newNoToSkip
                                    where
                                      (maybeCell, newNoToSkip) = emptyCellRowHelper r noToSkip


  emptyCellRowHelper :: [Cell] -> Int -> (Maybe Cell, Int)
  emptyCellRowHelper (c:cells) 0                  | (value c) == 0 = (Just c, 0)
                                                  | otherwise = emptyCellRowHelper cells 0
  emptyCellRowHelper [] 0                         = (Nothing, 0)
  emptyCellRowHelper (c:cells) noAlreadyFilledIn  | (value c) == 0 = emptyCellRowHelper cells (noAlreadyFilledIn-1)
                                                  | otherwise = emptyCellRowHelper cells noAlreadyFilledIn


  -- Given a sudoku, a cell and an int, updates the given cell with the given int in the given sudoku and returns the new sudoku
  changeCellValue :: Sudoku -> Cell -> Int -> Sudoku
  changeCellValue [] _ _ = []
  changeCellValue (r:rows) cell value = (changeCellValueRow r cell value) : (changeCellValue rows cell value)

  -- Given a list of cells, a cell and an int, updates the given cell with the given int in the given list of cells and returns the new list of cells
  changeCellValueRow :: [Cell] -> Cell -> Int -> [Cell]
  changeCellValueRow [] _ _ = []
  changeCellValueRow (c:cells) cell value
  									| (coords cell) == (coords c) = cell {value = value} : cells
  									| otherwise = c : (changeCellValueRow cells cell value)

  generateEmptySudoku :: Int -> Sudoku
  generateEmptySudoku n = generateEmptySudoku' n  n []

  generateEmptySudoku' :: Int -- no. of row
                   -> Int -- no. of cols
                   -> [[Cell]] -- Partially generated sudoku
                   -> Sudoku
  generateEmptySudoku' 0 _ partial_sudoku = partial_sudoku
  generateEmptySudoku' rows cols partial_sudoku = generateEmptySudoku' (rows-1) cols (partial_sudoku ++ [(generateEmptyRow cols (cols-rows+1))])


  generateEmptyRow :: Int -> Int -> [Cell]
  generateEmptyRow cols rownum = generateEmptyRow' cols rownum []

  generateEmptyRow' :: Int -> Int -> [Cell] -> [Cell]
  generateEmptyRow' 0  _ intermediate = intermediate
  generateEmptyRow' rowwidth rownum intermediate = generateEmptyRow' (rowwidth-1) rownum  (intermediate ++ [cell])
                                                where
                                                    cellcolumn = (length intermediate)
                                                    blocknum = (truncate (fromIntegral rownum / 3))*10 + (truncate (fromIntegral cellcolumn /3))
                                                    cell = Cell{value = 0, coords = (rownum-1, cellcolumn), block = blocknum}

  -- solve :: Sudoku -> Sudoku
  -- solve sudoku = solve' sudoku 0
  --
  -- solve' :: Sudoku -> Int -> Sudoku
  -- solve' sudoku noFilledIn  | (length options) == 0 = --backtracken!!!
  --                           | otherwise =
  --                         where
  --                           firstEmpty = firstEmptyCell sudoku noFilledIn
  --                           options = possibleValues firstEmpty sudoku
