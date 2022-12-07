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
  , prevBoard :: V.Vector Board
  , graph :: V.Vector S.Node
  , cursorPos :: Coordinate
  , console :: V.Vector String
  , editList :: BWL.GenericList String V.Vector (C.CellContent, Name)
  }

getMode :: WorkState -> ModeState
getMode ws = mode ws

getBoard :: WorkState -> Board
getBoard ws = board ws

getPrevBoard :: WorkState -> V.Vector Board
getPrevBoard ws = prevBoard ws

getGraph :: WorkState -> V.Vector S.Node
getGraph ws = graph ws

getCursorPos :: WorkState -> Coordinate
getCursorPos ws = cursorPos ws

getConsole :: WorkState -> V.Vector String
getConsole ws = console ws

getEditList :: WorkState -> BWL.GenericList String V.Vector (C.CellContent, Name)
getEditList ws = editList ws

setMode :: WorkState -> ModeState -> WorkState
setMode ws ms = WS
  { mode = ms
  , board = getBoard ws
  , prevBoard = getPrevBoard ws
  , graph = getGraph ws
  , cursorPos = getCursorPos ws
  , console = getConsole ws
  , editList = getEditList ws
  }

setBoard :: WorkState -> Board -> WorkState
setBoard ws b = WS
  { mode = getMode ws
  , board = b
  , prevBoard = getPrevBoard ws
  , graph = getGraph ws
  , cursorPos = getCursorPos ws
  , console = getConsole ws
  , editList = getEditList ws
  }

setPrevBoard :: WorkState -> V.Vector Board -> WorkState
setPrevBoard ws pb = WS
  { mode = getMode ws
  , board = getBoard ws
  , prevBoard = pb
  , graph = getGraph ws
  , cursorPos = getCursorPos ws
  , console = getConsole ws
  , editList = getEditList ws
  }

setGraph :: WorkState -> V.Vector S.Node -> WorkState
setGraph ws g = WS
  { mode = getMode ws
  , board = getBoard ws
  , prevBoard = getPrevBoard ws
  , graph = g
  , cursorPos = getCursorPos ws
  , console = getConsole ws
  , editList = getEditList ws
  }

setCursorPos :: WorkState -> Coordinate -> WorkState
setCursorPos ws cp = WS
  { mode = getMode ws
  , board = getBoard ws
  , prevBoard = getPrevBoard ws
  , graph = getGraph ws
  , cursorPos = cp
  , console = getConsole ws
  , editList = getEditList ws
  }

setConsole :: WorkState -> V.Vector String -> WorkState
setConsole ws vs = WS
  { mode = getMode ws
  , board = getBoard ws
  , prevBoard = getPrevBoard ws
  , graph = getGraph ws
  , cursorPos = getCursorPos ws
  , console = vs
  , editList = getEditList ws
  }

setEditList :: WorkState -> BWL.GenericList String V.Vector (C.CellContent, Name) -> WorkState
setEditList ws el = WS
  { mode = getMode ws
  , board = getBoard ws
  , prevBoard = getPrevBoard ws
  , graph = getGraph ws
  , cursorPos = getCursorPos ws
  , console = getConsole ws
  , editList = el
  }

toRun :: WorkState -> WorkState
toRun ws = modifyBoard ws (setBoard ws (V.foldl (\b (coord, bool) -> if bool then M.setElem (C.C { content = C.HighOutput, coordinate = coord }) coord b else M.setElem (C.C { content = C.LowOutput, coordinate = coord }) coord b) b (V.zip outList solvedGraphs)))
  where
    b = getBoard ws
    outList = getOutputList b
    g = getGraph ws
    solvedGraphs = V.map (\graph -> S.solve graph) g

setCellAtCursorPosWithCell :: WorkState -> C.CellContent -> WorkState
setCellAtCursorPosWithCell ws cc = setBoard ws (M.setElem (C.setContent (M.getElem y x b) cc) (y, x) b)
  where
    b = getBoard ws
    (y, x) = getCursorPos ws

setCellToSelectedEditListCell :: WorkState -> WorkState
setCellToSelectedEditListCell ws = modifyBoard ws (setCellAtCursorPosWithCell ws selectedCell)
  where
    selectedCell = getEditListSelectedCell ws

setCellToEmpty :: WorkState -> WorkState
setCellToEmpty ws = modifyBoard ws (setCellAtCursorPosWithCell ws C.Empty)

setCellToRotate :: WorkState -> WorkState
setCellToRotate ws = modifyBoard ws (setBoard ws (M.setElem (C.rotate currCell) (y, x) b))
  where
    b = getBoard ws
    (y, x) = getCursorPos ws
    currCell = M.getElem y x b

modifyBoard :: WorkState -> WorkState -> WorkState
modifyBoard ws wsNew = setPrevBoard wsNew (addPrevBoard (getPrevBoard ws) (getBoard ws))

setBoardToPrev :: WorkState -> WorkState
setBoardToPrev ws
  | V.length (getPrevBoard ws) > 0 = setPrevBoard (setBoard ws (V.head (getPrevBoard ws))) (V.tail (getPrevBoard ws))
  | otherwise = ws

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

addConsoleMessage :: WorkState -> String -> WorkState
addConsoleMessage ws s = setConsole ws (addToConsole (getConsole ws) s)

addConsoleMessages :: WorkState -> V.Vector String -> WorkState
addConsoleMessages ws vs = setConsole ws (addMultiToConsole (getConsole ws) vs)

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
  , prevBoard = initPrevBoard
  , graph = initGraph
  , cursorPos = initCursorPos
  , console = initConsole
  , editList = initEditList
  }

initLoadWorkState :: Board -> WorkState
initLoadWorkState b = WS
  { mode = View
  , board = b
  , prevBoard = initPrevBoard
  , graph = initGraph
  , cursorPos = initCursorPos
  , console = initConsole
  , editList = initEditList
  }

data ModeState
  = View
  | Edit
  | Run

type Board = M.Matrix C.Cell

getOutputList :: Board -> V.Vector Coordinate
getOutputList b = Prelude.foldl (\v c -> case (C.getContent c) of { UnknownOutput -> V.snoc v (C.getCoordinate c); otherwise -> v }) V.empty b

initBoard :: Board
initBoard = M.matrix boardSize boardSize (\c -> C.C { content = C.Empty, coordinate = c })

addPrevBoard :: V.Vector Board -> Board -> V.Vector Board
addPrevBoard pb b = V.take prevBoardSize (cons b pb)

initPrevBoard :: V.Vector Board
initPrevBoard = V.empty

initGraph :: V.Vector S.Node
initGraph = V.empty

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

--TODO: Do console with take instead of yucky logic
addToConsole :: V.Vector String -> String -> V.Vector String
addToConsole c s
  | V.length c < consoleSize = V.snoc c s
  | otherwise = V.tail (V.snoc c s)

addMultiToConsole :: V.Vector String -> V.Vector String -> V.Vector String
addMultiToConsole c vs
  | (V.length newC) <= consoleSize = newC
  | otherwise = V.slice ((V.length newC) - consoleSize) consoleSize newC
  where
    newC = c V.++ vs

initConsole :: V.Vector String
initConsole = V.replicate consoleSize " "

initEditList :: BWL.GenericList String V.Vector (C.CellContent, Name)
initEditList = BWL.list "editList" (C.placeableCellContents) 1

toFileString :: WorkState -> String
toFileString ws = Prelude.foldl (\s cell -> s Prelude.++ [(C.toFileEncoding (C.getContent cell))]) "" (getBoard ws)

fromFileString :: String -> WorkState
fromFileString s = initLoadWorkState (M.matrix boardSize boardSize (\(y, x) -> C.C {content = C.fromFileEncoding (s!!((y - 1) * boardSize + x - 1)), coordinate = (y, x) }))