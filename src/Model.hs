module Model where

import Data.Matrix as M

import Types
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
  , cursorPos :: Coordinate
  }

initWorkState :: WorkState
initWorkState = WS
  { mode = View
  , board = initBoard
  , cursorPos = initCursorPos
  }

data ModeState
  = View
  | Edit
  | Run

type Board = M.Matrix C.Cell

boardSize :: Int
boardSize = 30

initBoard :: Board
initBoard = M.matrix boardSize boardSize (\c -> C.C { content = C.Empty, coordinate = c })

initCursorPos :: Coordinate
initCursorPos = (div boardSize 2, div boardSize 2)