module Front where

import Prelude

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

-- data Cell = Cell {value :: Int, coords :: (Int, Int), block :: Int} -- position = (rownum, colnum)
        --   deriving (Show, Eq)

cellShape :: Cell -> [Shape]
cellShape cell = [Rectangle { position = (Point (fromIntegral (50*fst(coords cell)),fromIntegral (50*snd(coords cell)) ))
					   , dimensions = (50,50)
                       , fillColor = (0,0,0,0)
                       , strokeLineThickness = 1
                       , strokeColor = (0,0,0,1)
                       , rotationM = Nothing
                       },
				   Text { position = (Point (fromIntegral (50*fst(coords cell)+24),fromIntegral (50*snd(coords cell)+14) ))
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

changeText :: FillColor -> [Out]
changeText color
    = [ OutCanvas $ C.CanvasOperations canvasId [C.Clear C.ClearCanvas] -- Clear canvas completely
      , OutBasicShapes $ DrawShapes canvasId sudoku -- Draw Text shape
      ]
    where
        sudoku = sudokuShape sudokuTest

sudokuTest = generateEmptySudoku 9


config = defaultConfig { setupModuleConfigurations=[ C.setupCanvasModuleConfiguration
                                                       , setupBasicShapesModuleConfiguration
                                                       , setupMouseModuleConfiguration
                                                       , setupKeyboardModuleConfiguration
                                                       ]}
                where
                    defaultConfig = allModulesEventloopSetupConfiguration beginProgramState eventloop


-- type LastSelectedCell = Maybe Cell

-- data ProgramState = ProgramState { sudoku :: Sudoku, selected :: LastSelectedCell }
-- 				  deriving (Eq, Show)

data ProgramState = ProgramState { colors :: [FillColor] }
                  deriving (Eq, Show)

-- beginProgramState = ProgramState { sudoku = generateSudoku, selected = Nothing }

beginProgramState = ProgramState colorCycle

colorCycle :: [FillColor] -- (FillColor == Color == (Red, Green, Blue, Alpha))
colorCycle = cycle [ (255, 0, 0, 255) -- Red
                   , (0, 255, 0, 255) -- Green
                   , (0, 0, 255, 255) -- Blue
                   , (255, 0, 255, 255) -- Purple
                   , (255, 180, 0, 255) -- Orange
                   ]





eventloop :: ProgramState -> In -> (ProgramState, [Out])
eventloop ps Start
    = (ps', setupCanvas:(changeText nextColor))
    where
        setupCanvas = OutCanvas $ C.SetupCanvas canvasId 1 (512, 512) (C.CSSPosition C.CSSFromCenter (C.CSSPercentage 50, C.CSSPercentage 50))
        (nextColor:rest) = colors ps
        ps' = ps {colors = rest}

eventloop ps (InMouse (Mouse MouseCanvas canvasId (Click MouseLeft) p))
    = (ps', changeText nextColor)
    where
        (nextColor:rest) = colors ps
        ps' = ps {colors = rest}


eventloop ps (InKeyboard (Key "c"))
    = (ps', changeText nextColor)
    where
        (nextColor:rest) = colors ps
        ps' = ps {colors = rest}

eventloop ps (InKeyboard (Key "s")) = (ps, [Stop]) -- Stop the program when the letter 's' is pressed

eventloop ps _ = (ps, []) -- Very important to avoid errors on In events that aren't handled but expected!




main = startEventloopSystem config
