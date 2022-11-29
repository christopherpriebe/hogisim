module Model where

import Data.Matrix as M
import Data.Vector as V
import Data.Maybe
import Brick.Widgets.List as BWL

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
  , editList :: BWL.GenericList String V.Vector (C.CellContent, Name)
  }

initWorkState :: WorkState
initWorkState = WS
  { mode = View
  , board = initBoard
  , cursorPos = initCursorPos
  , editList = initEditList
  }

data ModeState
  = View
  | Edit
  | Run

type Board = M.Matrix C.Cell

initBoard :: Board
initBoard = M.matrix boardSize boardSize (\c -> C.C { content = C.Empty, coordinate = c })

initCursorPos :: Coordinate
initCursorPos = (div boardSize 2, div boardSize 2)

getListSelectedElement :: BWL.GenericList String V.Vector (C.CellContent, Name) -> C.CellContent
getListSelectedElement el =
  case BWL.listSelectedElement el of
    Just tup -> fst (snd tup)
    Nothing -> C.Empty

initEditList :: BWL.GenericList String V.Vector (C.CellContent, Name)
initEditList = BWL.list "editList" (C.placeableCellContents) 1