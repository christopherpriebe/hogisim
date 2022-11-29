module Control where

import Data.Matrix as M --TODO: Delete
import Brick as B
import Brick.Widgets.List as BWL
import Graphics.Vty as V

import Model
import Model.Cell as C --TODO: Delete
import Types

handleEvent :: AppState -> B.BrickEvent String Name -> B.EventM Name (Next AppState)
handleEvent (Menu New) (B.VtyEvent (V.EvKey V.KUp [])) = B.continue (Menu Exit)
handleEvent (Menu Load) (B.VtyEvent (V.EvKey V.KUp [])) = B.continue (Menu New)
handleEvent (Menu Exit) (B.VtyEvent (V.EvKey V.KUp [])) = B.continue (Menu Load)
handleEvent (Menu New) (B.VtyEvent (V.EvKey V.KDown [])) = B.continue (Menu Load)
handleEvent (Menu Load) (B.VtyEvent (V.EvKey V.KDown [])) = B.continue (Menu Exit)
handleEvent (Menu Exit) (B.VtyEvent (V.EvKey V.KDown [])) = B.continue (Menu New)

handleEvent (Menu New) (B.VtyEvent (V.EvKey V.KEnter [])) = B.continue (Work initWorkState)
handleEvent (Menu Load) (B.VtyEvent (V.EvKey V.KEnter [])) = B.continue (Menu Load)
handleEvent (Menu Exit) (B.VtyEvent (V.EvKey V.KEnter [])) = B.halt (Menu Exit)

handleEvent (Work (WS { mode = View, board = b, cursorPos = cp, editList = el })) (B.VtyEvent (V.EvKey (V.KChar '2') [])) = B.continue (Work (WS { mode = Edit, board = b, cursorPos = cp, editList = el }))
handleEvent (Work (WS { mode = Edit, board = b, cursorPos = cp, editList = el })) (B.VtyEvent (V.EvKey (V.KChar '1') [])) = B.continue (Work (WS { mode = View, board = b, cursorPos = cp, editList = el }))
handleEvent (Work (WS { mode = View, board = b, cursorPos = cp, editList = el })) (B.VtyEvent (V.EvKey (V.KChar '3') [])) = B.continue (Work (WS { mode = Run, board = b, cursorPos = cp, editList = el }))
handleEvent (Work (WS { mode = Run, board = b, cursorPos = cp, editList = el })) (B.VtyEvent (V.EvKey (V.KChar '1') [])) = B.continue (Work (WS { mode = View, board = b, cursorPos = cp, editList = el }))

handleEvent (Work (WS { mode = m, board = b, cursorPos = (y, x), editList = el })) (B.VtyEvent (V.EvKey V.KRight [])) = B.continue (Work (WS { mode = m, board = b, cursorPos = (y, x + 1), editList = el }))
handleEvent (Work (WS { mode = m, board = b, cursorPos = (y, x), editList = el })) (B.VtyEvent (V.EvKey V.KLeft [])) = B.continue (Work (WS { mode = m, board = b, cursorPos = (y, x - 1), editList = el }))
handleEvent (Work (WS { mode = m, board = b, cursorPos = (y, x), editList = el })) (B.VtyEvent (V.EvKey V.KUp [])) = B.continue (Work (WS { mode = m, board = b, cursorPos = (y - 1, x), editList = el }))
handleEvent (Work (WS { mode = m, board = b, cursorPos = (y, x), editList = el })) (B.VtyEvent (V.EvKey V.KDown [])) = B.continue (Work (WS { mode = m, board = b, cursorPos = (y + 1, x), editList = el }))

handleEvent (Work (WS { mode = Edit, board = b, cursorPos = (y, x), editList = el })) (B.VtyEvent (V.EvKey V.KUp [V.MShift])) = B.continue (Work (WS { mode = Edit, board = b, cursorPos = (y, x), editList = BWL.listMoveBy (-1) el }))
handleEvent (Work (WS { mode = Edit, board = b, cursorPos = (y, x), editList = el })) (B.VtyEvent (V.EvKey V.KDown [V.MShift])) = B.continue (Work (WS { mode = Edit, board = b, cursorPos = (y, x), editList = BWL.listMoveBy 1 el }))
handleEvent (Work (WS { mode = Edit, board = b, cursorPos = (y, x), editList = el })) (B.VtyEvent (V.EvKey (V.KChar 'f') [])) = B.continue (Work (WS { mode = Edit, board = M.setElem (C.setContent (M.getElem y x b) (getListSelectedElement el)) (y, x) b, cursorPos = (y, x), editList = el }))
handleEvent (Work (WS { mode = Edit, board = b, cursorPos = (y, x), editList = el })) (B.VtyEvent (V.EvKey (V.KChar 'r') [])) = B.continue (Work (WS { mode = Edit, board = M.setElem (C.rotate (M.getElem y x b)) (y, x) b, cursorPos = (y, x), editList = el }))

--handleEvent (Work (WS { mode = Edit, board = b, cursorPos = (y, x), editList = el })) (B.VtyEvent (V.EvKey V.KUp [V.MShift])) = B.continue (Work (WS { mode = Edit, board = M.setElem (C.nextContentForward (M.getElem y x b)) (y, x) b, cursorPos = (y, x), editList = el }))
--handleEvent (Work (WS { mode = Edit, board = b, cursorPos = (y, x), editList = el })) (B.VtyEvent (V.EvKey V.KDown [V.MShift])) = B.continue (Work (WS { mode = Edit, board = M.setElem (C.nextContentBackward (M.getElem y x b)) (y, x) b, cursorPos = (y, x), editList = el }))

handleEvent (Work _) (B.VtyEvent (V.EvKey V.KEsc [])) = B.continue (Menu New)

handleEvent s _ = B.continue s