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

-- data Cell = Cell {value :: Int, coords :: (Int, Int), block :: Int} -- coords = (rownum, colnum)
        --   deriving (Show, Eq)

cellShape :: Cell -> [Shape]
cellShape Cell {value = -1, coords = c} = [Rectangle { position = (Point (fromIntegral (50*snd(c)),fromIntegral (50*fst(c)) ))
					   , dimensions = (50,50)
                       , fillColor = (0,0,0,0)
                       , strokeLineThickness = 1
                       , strokeColor = (0,0,0,1)
                       , rotationM = Nothing
                       },
				   Text { position = (Point (fromIntegral (50*snd(c)+24),fromIntegral (50*fst(c)+14) ))
				   	   , text = ""
					   , fontFamily = "Arial"
					   , fontSize = 30
					   , alignment = AlignCenter
                       , fillColor = (0,0,0,1)
                       , strokeLineThickness = 1
                       , strokeColor = (0,0,0,0)
                       , rotationM = Nothing
                       }]
cellShape cell = [Rectangle { position = (Point (fromIntegral (50*snd(coords cell)),fromIntegral (50*fst(coords cell)) ))
					   , dimensions = (50,50)
                       , fillColor = (0,0,0,0)
                       , strokeLineThickness = 1
                       , strokeColor = (0,0,0,1)
                       , rotationM = Nothing
                       },
				   Text { position = (Point (fromIntegral (50*snd(coords cell)+24),fromIntegral (50*fst(coords cell)+14) ))
				   	   , text = show (value cell)
					   , fontFamily = "Arial"
					   , fontSize = 30
					   , alignment = AlignCenter
                       , fillColor = (0,0,0,1)
                       , strokeLineThickness = 1
                       , strokeColor = (0,0,0,0)
                       , rotationM = Nothing
                       }]

sudokuShape :: Sudoku -> [Shape]
sudokuShape [] = []
sudokuShape (r:rows) = (rowShape r) ++ (sudokuShape rows)

rowShape :: [Cell] -> [Shape]
rowShape [] = []
rowShape (c:cells) = (cellShape c) ++ (rowShape cells)

redraw :: Sudoku -> [Out]
redraw sudoku
    = [ OutCanvas $ C.CanvasOperations canvasId [C.Clear C.ClearCanvas] -- Clear canvas completely
      , OutBasicShapes $ DrawShapes canvasId sudoku' -- Draw Text shape
      ]
    where
        sudoku' = sudokuShape sudoku

--------------------------------------------- Utility functions ---------------------------------------

clickedCell :: Sudoku -> Point -> Maybe Cell
clickedCell [] _ = Nothing
clickedCell (r:rows) p
					| foundMaybeCell == Nothing = clickedCell rows p
					| otherwise = foundMaybeCell
				where
					foundMaybeCell = clickedCellInRow r p

clickedCellInRow :: [Cell] -> Point -> Maybe Cell
clickedCellInRow [] _ = Nothing
clickedCellInRow (c:cells) p
						| round (x p) > (50*(snd (coords c)-1)) && round (x p) < (50*(snd (coords c)+1)) && round (y p) > (50*(fst (coords c)-1)) && round (y p) < (50*(fst (coords c)+1)) = Just c
						| otherwise = clickedCellInRow cells p

changeCellValue :: Sudoku -> Cell -> Int -> Sudoku
changeCellValue [] _ _ = []
changeCellValue (r:rows) cell value = (changeCellValueRow r cell value) : (changeCellValue rows cell value)

changeCellValueRow :: [Cell] -> Cell -> Int -> [Cell]
changeCellValueRow [] _ _ = []
changeCellValueRow (c:cells) cell value
									| (coords cell) == (coords c) = Cell {value = value, coords = (coords c), block = (block c)} : cells
									| otherwise = c : (changeCellValueRow cells cell value)
--------------------------------------------- Interfacing ---------------------------------------------

type LastSelectedCell = Maybe Cell

data ProgramState = ProgramState { sudoku :: Sudoku, selected :: LastSelectedCell }
				  deriving (Eq, Show)

beginProgramState = ProgramState { sudoku = generateEmptySudoku 9, selected = Nothing }

eventloop :: ProgramState -> In -> (ProgramState, [Out])
eventloop ps Start
    = (ps, setupCanvas:(sudokuOut))
    where
        setupCanvas = OutCanvas $ C.SetupCanvas canvasId 1 (512, 512) (C.CSSPosition C.CSSFromCenter (C.CSSPercentage 50, C.CSSPercentage 50))
        sudokuOut = redraw (sudoku ps)

eventloop ps (InMouse (Mouse MouseCanvas canvasId (Click MouseLeft) p))
    = (ps', sudokuOut)
    where
		sudokuOut = redraw (sudoku ps)
		ps' = ProgramState {sudoku = (sudoku ps), selected = selectedCell}
		selectedCell = clickedCell (sudoku ps) p

eventloop ProgramState {sudoku = s, selected = Just cell} (InKeyboard (Key char))
    = (ps', sudokuOut)
    where
		newSudoku = changeCellValue s cell (digitToInt $ char !! 0)
		sudokuOut = redraw newSudoku
		ps' = ProgramState {sudoku = newSudoku, selected = (Just cell)}

eventloop ps (InKeyboard (Key "p")) = error (show (selected ps))
eventloop ps (InKeyboard (Key "s")) = (ps, [Stop]) -- Stop the program when the letter 's' is pressed

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
