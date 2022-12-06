module Model where

import Data.Matrix as M
import Data.Vector as V
import Data.Maybe
import Brick.Widgets.List as BWL

import Types
import Model.Cell as C
import Sim as S

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

getMode :: WorkState -> ModeState
getMode ws = mode ws

getBoard :: WorkState -> Board
getBoard ws = board ws

getCursorPos :: WorkState -> Coordinate
getCursorPos ws = cursorPos ws

getEditList :: WorkState -> BWL.GenericList String V.Vector (C.CellContent, Name)
getEditList ws = editList ws

setMode :: WorkState -> ModeState -> WorkState
setMode ws ms = WS
  { mode = ms
  , board = getBoard ws
  , cursorPos = getCursorPos ws
  , editList = getEditList ws
  }

setBoard :: WorkState -> Board -> WorkState
setBoard ws b = WS
  { mode = getMode ws
  , board = b
  , cursorPos = getCursorPos ws
  , editList = getEditList ws
  }

setCursorPos :: WorkState -> Coordinate -> WorkState
setCursorPos ws cp = WS
  { mode = getMode ws
  , board = getBoard ws
  , cursorPos = cp
  , editList = getEditList ws
  }

setEditList :: WorkState -> BWL.GenericList String V.Vector (C.CellContent, Name) -> WorkState
setEditList ws el = WS
  { mode = getMode ws
  , board = getBoard ws
  , cursorPos = getCursorPos ws
  , editList = el
  }

setCellAtCursorPosWithCell :: WorkState -> C.CellContent -> WorkState
setCellAtCursorPosWithCell ws cc = setBoard ws (M.setElem (C.setContent (M.getElem y x b) cc) (y, x) b)
  where
    b = getBoard ws
    (y, x) = getCursorPos ws

setCellToSelectedEditListCell :: WorkState -> WorkState
setCellToSelectedEditListCell ws = setCellAtCursorPosWithCell ws selectedCell
  where
    selectedCell = getEditListSelectedCell ws

setCellToEmpty :: WorkState -> WorkState
setCellToEmpty ws = setCellAtCursorPosWithCell ws C.Empty

setCellToRotate :: WorkState -> WorkState
setCellToRotate ws = setBoard ws (M.setElem (C.rotate currCell) (y, x) b)
  where
    b = getBoard ws
    (y, x) = getCursorPos ws
    currCell = M.getElem y x b

moveCursorPosRight :: WorkState -> WorkState
moveCursorPosRight ws = moveCursorPos ws cursorRight

moveCursorPosLeft :: WorkState -> WorkState
moveCursorPosLeft ws = moveCursorPos ws cursorLeft

moveCursorPosUp :: WorkState -> WorkState
moveCursorPosUp ws = moveCursorPos ws cursorUp

moveCursorPosDown :: WorkState -> WorkState
moveCursorPosDown ws = moveCursorPos ws cursorDown

moveCursorPos :: WorkState -> (Coordinate -> Coordinate) -> WorkState
moveCursorPos ws f = setCursorPos ws (f (getCursorPos ws))

moveEditListCursorUp :: WorkState -> WorkState
moveEditListCursorUp ws = setEditList ws (BWL.listMoveBy (-1) (getEditList ws))

moveEditListCursorDown :: WorkState -> WorkState
moveEditListCursorDown ws = setEditList ws (BWL.listMoveBy (1) (getEditList ws))

getEditListSelectedCell :: WorkState -> C.CellContent
getEditListSelectedCell ws =
  case BWL.listSelectedElement (getEditList ws) of
    Just tup -> fst (snd tup)
    Nothing -> C.Empty

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

cursorRight :: Coordinate -> Coordinate
cursorRight (y, x) = if x < boardSize then (y, x + 1) else (y, x)

cursorLeft :: Coordinate -> Coordinate
cursorLeft (y, x) = if x > 1 then (y, x - 1) else (y, x)

cursorUp :: Coordinate -> Coordinate
cursorUp (y, x) = if y > 1 then (y - 1, x) else (y, x)

cursorDown :: Coordinate -> Coordinate
cursorDown (y, x) = if y < boardSize  then (y + 1, x) else (y, x)

initCursorPos :: Coordinate
initCursorPos = (div boardSize 2, div boardSize 2)

initEditList :: BWL.GenericList String V.Vector (C.CellContent, Name)
initEditList = BWL.list "editList" (C.placeableCellContents) 1