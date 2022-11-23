module Control where

import Brick as B
import Graphics.Vty as V

import Model
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

handleEvent (Work (WS { mode = View, board = b, cursorPos = (y, x) })) (B.VtyEvent (V.EvKey V.KRight [])) = B.continue (Work (WS { mode = View, board = b, cursorPos = (y, x + 1) }))
handleEvent (Work (WS { mode = View, board = b, cursorPos = (y, x) })) (B.VtyEvent (V.EvKey V.KLeft [])) = B.continue (Work (WS { mode = View, board = b, cursorPos = (y, x - 1) }))
handleEvent (Work (WS { mode = View, board = b, cursorPos = (y, x) })) (B.VtyEvent (V.EvKey V.KUp [])) = B.continue (Work (WS { mode = View, board = b, cursorPos = (y - 1, x) }))
handleEvent (Work (WS { mode = View, board = b, cursorPos = (y, x) })) (B.VtyEvent (V.EvKey V.KDown [])) = B.continue (Work (WS { mode = View, board = b, cursorPos = (y + 1, x) }))
handleEvent (Work _) (B.VtyEvent (V.EvKey V.KEsc [])) = B.continue (Menu New)

handleEvent s _ = B.continue s