module Logic where
  import Data.List
  import Data.Maybe
  import Debug.Trace

  type Sudoku = [[Cell]]

  data Cell = Cell {value :: Int, coords :: (Int, Int), block :: Int}
-- coords = (rownum, colnum)
            deriving (Show, Eq)

  blockPeers :: Cell -> Sudoku -> [Cell]
  blockPeers _ [] = []
  blockPeers cell (row:rows) = (blockPeers cell  rows) ++ (rowBlockPeers cell row)

  rowBlockPeers :: Cell -> [Cell] -> [Cell]
  rowBlockPeers cell [] = []
  rowBlockPeers cell (c:cells)  | (block cell) == (block c) && cell /= c = (rowBlockPeers cell cells) ++ [c]
                                | otherwise = rowBlockPeers cell cells

  rowPeers :: Cell -> Sudoku -> [Cell]
  rowPeers cell sudoku = ys ++ (tail zs)
                      where
                        rownum = fst (coords cell)
                        colnum = snd (coords cell)
                        complete_row = sudoku !! rownum
                        (ys, zs) = splitAt colnum complete_row

  colPeers :: Cell -> Sudoku -> [Cell]
  colPeers _ [] = []
  colPeers cell (row:rows)  | cellRowNo == currentRowNo = colPeers cell rows
                            | otherwise = (colPeers cell rows) ++ [row !! cellColNo]
                            where
                              cellRowNo = fst (coords cell)
                              cellColNo = snd (coords cell)
                              firstOfRow = head
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

  firstEmptyCell :: Sudoku -> Int -> Maybe Cell
  firstEmptyCell [] _ = Nothing
  firstEmptyCell (r:rows) noToSkip  =  case maybeCell of
                                    Just cell -> Just cell
                                    Nothing -> firstEmptyCell rows newNoToSkip
                                    where
                                      (maybeCell, newNoToSkip) = emptyCellRowHelper r noToSkip


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

  generateEmptySudoku :: Int -> Sudoku
  generateEmptySudoku n = generateEmptySudoku' n  n []

  generateEmptySudoku' :: Int -- no. of row
                   -> Int -- no. of cols
                   -> [[Cell]] -- Partially generated sudoku
                   -> Sudoku
  generateEmptySudoku' 0 _ partial_sudoku = partial_sudoku
  generateEmptySudoku' rows cols partial_sudoku = generateEmptySudoku' (rows-1) cols (partial_sudoku ++ [(generateEmptyRow cols (cols-rows+1))])

  -- generateEmptySudoku :: Int -> Sudoku -- generates nxn sudoku with all values set to 0.
  -- generateEmptySudoku 0 = []
  -- generateEmptySudoku n = (generateEmptySudoku (n-1)) ++ [(generateEmptyRow n n)]

  -- generateEmptyRow :: Int -> [Cell]
  -- generateEmptyRow n  = (generateEmptyRow n-1) ++ [cell]
  --                     where
  --                         cell = Cell{value = 0, coords = (rownum-1, cellcolumn), block = blocknum}
  --                         cellcolumn = (length (generateEmptyRow n-1))
  --                         blocknum = (truncate (fromIntegral n / 3))*10 + (truncate (fromIntegral cellcolumn /3))


  generateEmptyRow :: Int -> Int -> [Cell]
  generateEmptyRow cols rownum = generateEmptyRow' cols rownum []

  generateEmptyRow' :: Int -> Int -> [Cell] -> [Cell]
  generateEmptyRow' 0  _ intermediate = intermediate
  generateEmptyRow' rowwidth rownum intermediate = generateEmptyRow' (rowwidth-1) rownum  (intermediate ++ [cell])
                                                where
                                                    cellcolumn = (length intermediate)
                                                    blocknum = (truncate (fromIntegral rownum / 3))*10 + (truncate (fromIntegral cellcolumn /3))
                                                    cell = Cell{value = 0, coords = (rownum-1, cellcolumn), block = blocknum}


  applyPartialSolution :: Sudoku -> [Int] -> Sudoku -- List of ints is values for partial solution
  applyPartialSolution sudoku [] = sudoku
  applyPartialSolution sudoku (x:xs)  = case nextEmpty of
                                      Just cell -> let
                                        newSudoku = changeCellValue sudoku cell x
                                        in applyPartialSolution newSudoku xs
                                      Nothing -> error "Too many arguments applied to applyPartialSolution"       -- geen lege cellen meer over, sudoku is vol!
                                    where
                                      nextEmpty = firstEmptyCell sudoku (length (x:xs))


  solve :: Sudoku -> Sudoku
  solve sudoku = solve' sudoku [] Nothing

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
                                                        Nothing     -> (applyPartialSolution sudoku partialSolution)
                                                      where
                                                          nextEmpty = firstEmptyCell sudoku (length partialSolution)
                                                          -- init & lst commando's uitgevoerd op originele lijst of niet?





    -- case maybeOptionList of
    --                                               Just optionList -> case nextEmpty of
    --                                                                 Just cell   | (length options) == 0 -> solve' sudoku (init partialSolution) (Just trimmedOptions)     --backtracken!!!
    --                                                                             | otherwise -> solve' sudoku (partialSolution ++ [head optionList]) Nothing
    --                                                                 Nothing     -> sudoku
    --
    --                                               Nothing ->        case nextEmpty of
    --                                                                 Just cell   | (length options) == 0 -> solve' sudoku (init partialSolution) (Just trimmedOptions)    --backtracken!!!
    --                                                                             | otherwise -> solve' sudoku (partialSolution ++ [head options]) Nothing
    --                                                                 Nothing     -> sudoku                                                             -- geen lege cellen meer, sudoku compleet.
    --                                               where
    --                                                 nextEmpty = firstEmptyCell sudoku (length partialSolution)
    --                                                 options = possibleValues nextEmpty (applyPartialSolution sudoku partialSolution)
    --                                                 trimmedOptions = filter (/= (last partialSolution)) (possibleValues nextEmpty (applyPartialSolution sudoku (init partialSolution)))
    --                                                 -- init & lst commando's uitgevoerd op originele lijst of niet?
