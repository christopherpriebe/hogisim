module Model where

import Data.Vector as V
import Model.Cell as C

data AppState
  = Menu MenuState
  | Work WorkState

initAppState :: AppState
initAppState = Menu initMenuState

data MenuState
  = New
  | Load
  | Exit

initMenuState :: MenuState
initMenuState = New

data WorkState = WS
  { mode :: ModeState
  , board :: Board
  }

initWorkState :: WorkState
initWorkState = WS
  { mode = View
  , board = initBoard
  }

data ModeState
  = View
  | Edit
  | Run

type Board = V.Vector C.Cell

boardSize :: Int
boardSize = 20

initBoard :: Board
initBoard = V.replicate (boardSize * boardSize) C.Empty

newlineFunctionList :: [String -> Cell -> String]
newlineFunctionList = Prelude.cycle ((Prelude.replicate (boardSize - 1) addCellToString) Prelude.++ [addCellToStringWithNewLine])

addCellToString :: String -> Cell -> String
addCellToString s c = s Prelude.++ (C.toString c)

addCellToStringWithNewLine :: String -> Cell -> String
addCellToStringWithNewLine s c = s Prelude.++ (C.toString c) Prelude.++ "\n"

toString :: Board -> String
toString b = Prelude.foldl (\s (c, f) -> f s c) "" (Prelude.zip (V.toList b) newlineFunctionList)