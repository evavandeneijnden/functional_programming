module Front where

import Prelude
import Data.Char

import Logic

import Eventloop.Core
import Eventloop.DefaultConfiguration
import Eventloop.Types.Events
import Eventloop.Types.System

import qualified Eventloop.Module.Websocket.Canvas as C
import Eventloop.Module.Websocket.Mouse
import Eventloop.Module.Websocket.Keyboard
import Eventloop.Module.BasicShapes
import Eventloop.Utility.Vectors



canvasId :: C.CanvasId
canvasId = 1

---------------------------------- Drawing sudoku's ----------------------------------------------------

-- Takes a cell and returns the shapes on a canvas to draw that cell.

cellShape :: Cell -> [Shape]
-- In case a cell is empty (value = 0), show an empty cell
cellShape Cell {value = 0, coords = c} = [Rectangle { position = (Point (fromIntegral (50*snd(c)),fromIntegral (50*fst(c)) ))
					   , dimensions = (50,50)
                       , fillColor = (0,0,0,0)
                       , strokeLineThickness = 1
                       , strokeColor = (0,0,0,1)
                       , rotationM = Nothing
                       }] -- Square box indicating the actual cell
cellShape cell = [Rectangle { position = (Point (fromIntegral (50*snd(coords cell)),fromIntegral (50*fst(coords cell)) ))
					   , dimensions = (50,50)
                       , fillColor = (0,0,0,0)
                       , strokeLineThickness = 1
                       , strokeColor = (0,0,0,1)
                       , rotationM = Nothing
                       }, -- Square box indicating the cell
				   Text { position = (Point (fromIntegral (50*snd(coords cell)+24),fromIntegral (50*fst(coords cell)+14) ))
				   	   , text = show (value cell)
					   , fontFamily = "Arial"
					   , fontSize = 30
					   , alignment = AlignCenter
                       , fillColor = (0,0,0,1)
                       , strokeLineThickness = 1
                       , strokeColor = (0,0,0,0)
                       , rotationM = Nothing
                       }] -- Text indicating the value of the cell

-- Takes a sudoku and returns the shapes on a canvas to draw that sudoku.
sudokuShape :: Sudoku -> [Shape]
sudokuShape [] = []
sudokuShape (r:rows) = (rowShape r) ++ (sudokuShape rows)

-- Takes a list of cells (a row in the sudoku) and returns the shapes on a canvas to draw that list of cells.
rowShape :: [Cell] -> [Shape]
rowShape [] = []
rowShape (c:cells) = (cellShape c) ++ (rowShape cells)

-- Returns the shapes on a canvas to draw the solve-button
solveShape :: [Shape]
solveShape = [Rectangle { position = (Point (362, 462))
				   , dimensions = (100,50)
	               , fillColor = (0,0,0,0.36)
	               , strokeLineThickness = 2
	               , strokeColor = (0,0,0,1)
	               , rotationM = Nothing
	               },
			   Text { position = (Point (362+46,462+14))
			   	   , text = "Solve"
				   , fontFamily = "Arial"
				   , fontSize = 30
				   , alignment = AlignCenter
	               , fillColor = (0,0,0,1)
	               , strokeLineThickness = 1
	               , strokeColor = (0,0,0,0)
	               , rotationM = Nothing
	               }]

-- Returns the shapes on a canvas to draw the help-section
helpShape :: [Shape]
helpShape = [Text { position = (Point (0,462+14))
				, text = "Click a cell, then type a number to update that cell."
				, fontFamily = "Arial"
				, fontSize = 12
				, alignment = AlignLeft
				, fillColor = (0,0,0,1)
				, strokeLineThickness = 1
				, strokeColor = (0,0,0,0)
				, rotationM = Nothing
				},
			Text { position = (Point (0,462+14+20))
				, text = "Click the solve-button to solve the sudoku."
				, fontFamily = "Arial"
				, fontSize = 12
				, alignment = AlignLeft
				, fillColor = (0,0,0,1)
				, strokeLineThickness = 1
				, strokeColor = (0,0,0,0)
				, rotationM = Nothing
				}]
-- Redraw the canvas with the sudoku and the solve-button
redraw :: Sudoku -> [Out]
redraw sudoku
    = [ OutCanvas $ C.CanvasOperations canvasId [C.Clear C.ClearCanvas] -- Clear canvas completely
      , OutBasicShapes $ DrawShapes canvasId content -- Draw canvas again
      ]
    where
		content = solveShape ++ helpShape ++ sudoku'
		sudoku' = sudokuShape sudoku

--------------------------------------------- Utility functions ---------------------------------------

-- Given a sudoku and a point, finds out which cell (if any) of the sudoku is located at the given point
clickedCell :: Sudoku -> Point -> Maybe Cell
clickedCell [] _ = Nothing
clickedCell (r:rows) p
					| foundMaybeCell == Nothing = clickedCell rows p
					| otherwise = foundMaybeCell
				where
					foundMaybeCell = clickedCellInRow r p

-- Given a list of cells and a point, finds out which cell (if any) of that list is located at the given point
clickedCellInRow :: [Cell] -> Point -> Maybe Cell
clickedCellInRow [] _ = Nothing
clickedCellInRow (c:cells) p
						| round (x p) > (50*(snd (coords c)-1)) && round (x p) < (50*(snd (coords c)+1)) && round (y p) > (50*(fst (coords c)-1)) && round (y p) < (50*(fst (coords c)+1)) = Just c
						| otherwise = clickedCellInRow cells p


--------------------------------------------- Interfacing ---------------------------------------------

-- Used to indicate that last selected cell in the ProgramState
type LastSelectedCell = Maybe Cell

-- The state of the interface, consisting of the sudoku and the last selected cell
data ProgramState = ProgramState { sudoku :: Sudoku, selected :: LastSelectedCell }
				  deriving (Eq, Show)

-- Start state of the interface, an empty sudoku and no selected cell
beginProgramState = ProgramState { sudoku = generateEmptySudoku 9, selected = Nothing }

-- solve :: Sudoku -> Sudoku
-- solve s = changeCellValue s (s !! 6 !! 6) 36

-- Evaluated when the Start-event is fired: draws the canvas with the start sudoku
eventloop :: ProgramState -> In -> (ProgramState, [Out])
eventloop ps Start
    = (ps, setupCanvas:(sudokuOut))
    where
        setupCanvas = OutCanvas $ C.SetupCanvas canvasId 1 (512, 512) (C.CSSPosition C.CSSFromCenter (C.CSSPercentage 50, C.CSSPercentage 50))
        sudokuOut = redraw (sudoku ps)

-- Evaluated when a Left-Mouse-Button is clicked. Used to select a cell or select the solve-button with the mouse
eventloop ps (InMouse (Mouse MouseCanvas canvasId (Click MouseLeft) p))
	| (x p) > 362 && (x p) < 462 && (y p) > 462 && (y p) < 512 = (ps'2, redraw solvedSudoku)
    | otherwise = (ps'1, redraw (sudoku ps))
    where
		ps'1 = ProgramState {sudoku = (sudoku ps), selected = selectedCell}
		selectedCell = clickedCell (sudoku ps) p
		ps'2 = ProgramState {sudoku = solvedSudoku, selected = (selected ps)}
		solvedSudoku = solve (sudoku ps)

-- Evaluated when the letter s is pressed to stop the program
eventloop ps (InKeyboard (Key "s")) = (ps, [Stop])

-- Evaluated when a keypress is done. Used for number-inputs to update cell values
eventloop ProgramState {sudoku = s, selected = Just cell} (InKeyboard (Key char))
    = (ps', sudokuOut)
    where
		newSudoku = changeCellValue s cell (digitToInt $ char !! 0)
		sudokuOut = redraw newSudoku
		ps' = ProgramState {sudoku = newSudoku, selected = (Just cell)}

eventloop ps _ = (ps, []) -- Very important to avoid errors on In events that aren't handled but expected!

------------------------------------------------- Default stuff ------------------------------------------

config = defaultConfig { setupModuleConfigurations=[ C.setupCanvasModuleConfiguration
                                                       , setupBasicShapesModuleConfiguration
                                                       , setupMouseModuleConfiguration
                                                       , setupKeyboardModuleConfiguration
                                                       ]}
                where
                    defaultConfig = allModulesEventloopSetupConfiguration beginProgramState eventloop

main = startEventloopSystem config
