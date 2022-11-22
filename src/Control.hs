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

handleEvent (Work _) (B.VtyEvent (V.EvKey V.KEsc [])) = B.continue (Menu New)

handleEvent s _ = B.continue s