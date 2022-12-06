{-# LANGUAGE BlockArguments #-}

module Control where

import Brick as B
import Graphics.Vty as V

import Model as Model
import Types

handleEvent :: Model.AppState -> B.BrickEvent String Name -> B.EventM Name (Next Model.AppState)
handleEvent (Model.Menu m) (B.VtyEvent (V.EvKey V.KUp [])) = B.continue
  case m of
    Model.New -> Model.Menu Model.Exit
    Model.Load -> Model.Menu Model.New
    Model.Exit -> Model.Menu Model.Load
handleEvent (Model.Menu m) (B.VtyEvent (V.EvKey V.KDown [])) = B.continue
  case m of
    Model.New -> Model.Menu Model.Load
    Model.Load -> Model.Menu Model.Exit
    Model.Exit -> Model.Menu Model.New

handleEvent (Model.Menu Model.New) (B.VtyEvent (V.EvKey V.KEnter [])) = B.continue (Model.Work initWorkState)
handleEvent (Model.Menu Model.Load) (B.VtyEvent (V.EvKey V.KEnter [])) = B.suspendAndResume $ do
                                                                                    b <-readFile "out.hgs"
                                                                                    return (Model.Work initWorkState)
                                                                                    -- let loadedWorkState = WS {mode = View, board = b::Board, cursorPos = initCursorPos, editList = initEditList}
                                                                                    -- return (Work loadedWorkState)
handleEvent (Model.Menu Model.Exit) (B.VtyEvent (V.EvKey V.KEnter [])) = B.halt (Model.Menu Model.Exit)

handleEvent (Model.Work ws) (B.VtyEvent (V.EvKey V.KRight [])) = B.continue (Model.Work (Model.moveCursorPosRight ws))
handleEvent (Model.Work ws) (B.VtyEvent (V.EvKey V.KLeft [])) = B.continue (Model.Work (Model.moveCursorPosLeft ws))
handleEvent (Model.Work ws) (B.VtyEvent (V.EvKey V.KUp [])) = B.continue (Model.Work (Model.moveCursorPosUp ws))
handleEvent (Model.Work ws) (B.VtyEvent (V.EvKey V.KDown [])) = B.continue (Model.Work (Model.moveCursorPosDown ws))
handleEvent (Work _) (B.VtyEvent (V.EvKey V.KEsc [])) = B.continue (Menu New)
handleEvent (Model.Work ws) e =
  case (getMode ws) of
    Model.View -> handleViewEvent ws e
    Model.Edit -> handleEditEvent ws e
    Model.Run -> handleRunEvent ws e

handleViewEvent :: Model.WorkState -> B.BrickEvent String Name -> B.EventM Name (Next Model.AppState)
handleViewEvent ws (B.VtyEvent (V.EvKey (V.KChar '2') [])) = B.continue (Model.Work (Model.setMode ws Model.Edit))
handleViewEvent ws (B.VtyEvent (V.EvKey (V.KChar '3') [])) = B.continue (Model.Work (Model.setMode ws Model.Run))
handleViewEvent ws (B.VtyEvent (V.EvKey (V.KChar 's') [])) = B.suspendAndResume (do {writeFile "out.hgs" (show (Model.getBoard ws)); return (Model.Work ws)})
handleViewEvent ws _ = B.continue (Model.Work ws)

handleEditEvent :: Model.WorkState -> B.BrickEvent String Name -> B.EventM Name (Next Model.AppState)
handleEditEvent ws (B.VtyEvent (V.EvKey (V.KChar '1') [])) = B.continue (Model.Work (Model.setMode ws Model.View))
handleEditEvent ws (B.VtyEvent (V.EvKey V.KUp [V.MShift])) = B.continue (Model.Work (Model.moveEditListCursorUp ws))
handleEditEvent ws (B.VtyEvent (V.EvKey V.KDown [V.MShift])) = B.continue (Model.Work (Model.moveEditListCursorDown ws))
handleEditEvent ws (B.VtyEvent (V.EvKey (V.KChar 'f') [])) = B.continue (Model.Work (Model.setCellToSelectedEditListCell ws))
handleEditEvent ws (B.VtyEvent (V.EvKey (V.KChar 'r') [])) = B.continue (Model.Work (Model.setCellToRotate ws))
handleEditEvent ws (B.VtyEvent (V.EvKey (V.KChar 'd') [])) = B.continue (Model.Work (Model.setCellToEmpty ws))
handleEditEvent ws _ = B.continue (Model.Work ws)

handleRunEvent :: Model.WorkState -> B.BrickEvent String Name -> B.EventM Name (Next Model.AppState)
handleRunEvent ws (B.VtyEvent (V.EvKey (V.KChar '1') [])) = B.continue (Model.Work (Model.setMode ws Model.View))
handleRunEvent ws _ = B.continue (Model.Work ws)