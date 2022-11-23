module Model where

import Data.Vector as V

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

type Board = V.Vector (V.Vector C.Cell)

boardSize :: Int
boardSize = 30

initBoard :: Board
initBoard = V.map (\i -> V.slice (i * boardSize) boardSize init1DBoard) (V.generate boardSize (\i -> i))

init1DBoard :: Vector C.Cell
init1DBoard = (V.generate (boardSize * boardSize) (\i -> C { content = C.Empty, coordinate = quotRem i boardSize }))

initCursorPos :: Coordinate
initCursorPos = (div boardSize 2, div boardSize 2)