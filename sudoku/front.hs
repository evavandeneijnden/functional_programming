module Front where

import Prelude

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

cellShape :: Cell -> [Shape]
cellShape cell = [Rectangle { position = (Point (256, 256))
					   , dimensions = (50,50)
                       , fillColor = (0,0,0,0)
                       , strokeLineThickness = 1
                       , strokeColor = (0,0,0,1)
                       , rotationM = Nothing
                       },
				   Text { position = (Point (280, 270))
				   	   , text = "36"
					   , fontFamily = "Arial"
					   , fontSize = 30
					   , alignment = AlignCenter
                       , fillColor = (0,0,0,1)
                       , strokeLineThickness = 1
                       , strokeColor = (0,0,0,0)
                       , rotationM = Nothing
                       }]

changeText :: FillColor -> [Out]
changeText color
    = [ OutCanvas $ C.CanvasOperations canvasId [C.Clear C.ClearCanvas] -- Clear canvas completely
      , OutBasicShapes $ DrawShapes canvasId txtShape -- Draw Text shape
      ]
    where
        txtShape = textShape color




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
