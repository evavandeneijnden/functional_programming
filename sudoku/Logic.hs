module Logic where
  import Data.List
  import Data.Maybe

  -- A Sudoku is a matrix of cells
  type Sudoku = [[Cell]]

  -- Models a single space in a sudoku
  data Cell = Cell {value :: Int, coords :: (Int, Int), block :: Int}   -- coords = (rownum, colnum)
            deriving (Show, Eq)

  -- For a given cell, finds a list of cells that are in the same block (EXCLUDES the cell itself)
  blockPeers :: Cell -> Sudoku -> [Cell]
  blockPeers _ [] = []
  blockPeers cell (row:rows) = (blockPeers cell  rows) ++ (rowBlockPeers cell row)

  -- Helper for blockPeers function that finds a cell's peers in a given row
  rowBlockPeers :: Cell -> [Cell] -> [Cell]
  rowBlockPeers cell [] = []
  rowBlockPeers cell (c:cells)  | (block cell) == (block c) && cell /= c = (rowBlockPeers cell cells) ++ [c]
                                | otherwise = rowBlockPeers cell cells


  -- For a given cell, finds a list of cells that are in the same row (EXCLUDING the cell itself)
  rowPeers :: Cell -> Sudoku -> [Cell]
  rowPeers cell sudoku = ys ++ (tail zs)
                      where
                        rownum = fst (coords cell)
                        colnum = snd (coords cell)
                        complete_row = sudoku !! rownum
                        (ys, zs) = splitAt colnum complete_row

  -- For a given cellm finds a list of cells that are in the same column (EXCLUDING the cell itself)
  colPeers :: Cell -> Sudoku -> [Cell]
  colPeers _ [] = []
  colPeers cell (row:rows)  | cellRowNo == currentRowNo = colPeers cell rows
                            | otherwise = (colPeers cell rows) ++ [row !! cellColNo]
                            where
                              cellRowNo = fst (coords cell)
                              cellColNo = snd (coords cell)
                              firstOfRow = head
                              currentRowNo = fst (coords (head row))

  -- Given a cell, returns the possible values that that cell could have (by removing from a list of 1-9 all the options that its block- , row-, or columnpeers already have)
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

  -- Given a sudoku and a number of cells to skip
  -- (because these have been tried in a partial solution, but have not been added to the sudoku),
  -- returns the first cell for wich a value has yet to be found
  firstEmptyCell :: Sudoku -> Int -> Maybe Cell
  firstEmptyCell [] _ = Nothing
  firstEmptyCell (r:rows) noToSkip  =  case maybeCell of
                                    Just cell -> Just cell
                                    Nothing -> firstEmptyCell rows newNoToSkip
                                    where
                                      (maybeCell, newNoToSkip) = emptyCellRowHelper r noToSkip

  -- Helper that attempts to find the next empty cell within a row. If it is not found, it returns a tuple of (Nothing, n), where n is the number of cells that must still be skipped
  emptyCellRowHelper :: [Cell] -> Int -> (Maybe Cell, Int)
  emptyCellRowHelper (c:cells) 0                  | (value c) == 0 = (Just c, 0)
                                                  | otherwise = emptyCellRowHelper cells 0
  emptyCellRowHelper [] 0                         = (Nothing, 0)
  emptyCellRowHelper [] n                       = (Nothing, n)
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

  -- Generates an empty nxn Sudoku (that is, a Sudoku in which the value for each Cell is 0)
  generateEmptySudoku :: Int -> Sudoku
  generateEmptySudoku n = generateEmptySudoku' n  n []

  -- Helper for generating Sudoku. NOTE: Not 'neat' Haskell, but there was not enough time to fix it.
  generateEmptySudoku' :: Int -- no. of row
                   -> Int -- no. of cols
                   -> [[Cell]] -- Partially generated sudoku
                   -> Sudoku
  generateEmptySudoku' 0 _ partial_sudoku = partial_sudoku
  generateEmptySudoku' rows cols partial_sudoku = generateEmptySudoku' (rows-1) cols (partial_sudoku ++ [(generateEmptyRow cols (cols-rows+1))])

  -- Generates an empty row of n columns to function as a row in a Sudoku
  generateEmptyRow :: Int -> Int -> [Cell]
  generateEmptyRow cols rownum = generateEmptyRow' cols rownum []

  -- Helper for generating empty Sudoku row. NOTE: Again, not 'neat', but there was not enough time.
  generateEmptyRow' :: Int -> Int -> [Cell] -> [Cell]
  generateEmptyRow' 0  _ intermediate = intermediate
  generateEmptyRow' rowwidth rownum intermediate = generateEmptyRow' (rowwidth-1) rownum  (intermediate ++ [cell])
                                                where
                                                    cellcolumn = (length intermediate)
                                                    blocknum = (truncate (fromIntegral rownum / 3))*10 + (truncate (fromIntegral cellcolumn /3))
                                                    cell = Cell{value = 0, coords = (rownum-1, cellcolumn), block = blocknum}


  -- Function that converts a Sudoku and a partial solution (given as a list of ints, to be applied to the empty cells in order) into a new Sudoku
  applyPartialSolution :: Sudoku -> [Int] -> Sudoku -- List of ints is values for partial solution
  applyPartialSolution sudoku [] = sudoku
  applyPartialSolution sudoku (x:xs)  = case nextEmpty of
                                      Just cell -> let
                                        newSudoku = changeCellValue sudoku cell x
                                        in applyPartialSolution newSudoku xs
                                      Nothing -> error "Too many arguments applied to applyPartialSolution"       -- geen lege cellen meer over, sudoku is vol!
                                    where
                                      nextEmpty = firstEmptyCell sudoku (length (x:xs))

  -- Function that converts the original Sudoku and a list with the final solution into a new Sudoku.
  applyFinalSolution :: Sudoku -> [Int] -> Sudoku -- List of ints is values for partial solution
  applyFinalSolution sudoku [] = sudoku
  applyFinalSolution sudoku (x:xs)  = case nextEmpty of
                                      Just cell -> let
                                        newSudoku = changeCellValue sudoku cell x
                                        in applyFinalSolution newSudoku xs
                                      Nothing -> error "Too many arguments applied to applyPartialSolution"       -- geen lege cellen meer over, sudoku is vol!
                                    where
                                      nextEmpty = firstEmptyCell sudoku 0


  -- Function that takes a partially filled Sudoku and returns a fully solved one.
  solve :: Sudoku -> Sudoku
  solve sudoku = solve' sudoku [] Nothing

  -- Helper function for the solve function. NOTE: Once again, not 'neat' Haskell due to lack of time
  solve' :: Sudoku -> [Int] -> Maybe [Int] -> Sudoku
  solve' sudoku partialSolution maybeOptionList  =  case nextEmpty of
                                                        Just cell   | (length options) == 0 -> solve' sudoku (init partialSolution) (Just trimmedOptions)     --backtracken!!!
                                                                    | otherwise -> case maybeOptionList of
                                                                                      Just optionList | optionList == [] -> solve' sudoku (partialSolution ++ []) Nothing
                                                                                                      | otherwise -> solve' sudoku (partialSolution ++ [head optionList]) Nothing
                                                                                      Nothing         | options == [] -> solve' sudoku (partialSolution ++ []) Nothing
                                                                                                      | otherwise -> solve' sudoku (partialSolution ++ [head options]) Nothing
                                                                    where
                                                                      options = possibleValues cell (applyPartialSolution sudoku partialSolution)
                                                                      trimmedOptions = filter (/= (last partialSolution)) (possibleValues cell (applyPartialSolution sudoku (init partialSolution)))
                                                        Nothing     -> (applyFinalSolution sudoku partialSolution)
                                                      where
                                                          nextEmpty = firstEmptyCell sudoku (length partialSolution)
