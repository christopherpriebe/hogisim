{-# LANGUAGE BlockArguments #-}

module Control where

import Brick as B
import Graphics.Vty as V

import Model as Model
import Sim as Sim
import Types

import Data.Vector as Vec --delete

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
handleEvent (Model.Menu Model.Load) (B.VtyEvent (V.EvKey V.KEnter [])) = B.suspendAndResume
  do
    putStrLn "Please enter a relative file path and press \"Enter\" key: "
    filename <- getLine
    bRaw <- readFile filename
    return (Model.Work (Model.fromFileString bRaw))
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
handleViewEvent ws (B.VtyEvent (V.EvKey (V.KChar '2') [])) = B.continue (Model.Work newWS)
  where
    wsConsole = Model.addConsoleMessage ws "Switched from \"View\" to \"Edit\""
    wsMode = Model.setMode wsConsole Model.Edit
    newWS = wsMode
handleViewEvent ws (B.VtyEvent (V.EvKey (V.KChar '3') [])) = B.continue (Model.Work newWS)
  where
    wsConsole = Model.addConsoleMessage ws "Switched from \"View\" to \"Run\""
    wsMode = Model.setMode wsConsole Model.Run
    newWS = wsMode
handleViewEvent ws (B.VtyEvent (V.EvKey (V.KChar 's') [])) = B.suspendAndResume 
    do 
        putStrLn "Please enter a file name and press \"Enter\" key: "
        filename <- getLine;
        writeFile filename (Model.toFileString ws)
        -- writeFile "out.hgs" (Model.toFileString ws)
        return (Model.Work ws)
handleViewEvent ws _ = B.continue (Model.Work ws)

handleEditEvent :: Model.WorkState -> B.BrickEvent String Name -> B.EventM Name (Next Model.AppState)
handleEditEvent ws (B.VtyEvent (V.EvKey (V.KChar '1') [])) = B.continue (Model.Work newWS)
  where
    gEither = Vec.fromList (Sim.transform (Model.getBoard ws))
    isError = Vec.foldl (\b e -> case e of { Left _ -> True; Right _ -> if b then True else False }) False gEither
    errors = Vec.foldl (\a e -> case e of { Left err -> a Vec.++ (Vec.singleton (snd err)); Right _ -> a }) Vec.empty gEither
    g = Vec.map (\elem -> case elem of { Left e -> Sim.Input True; Right n -> n }) gEither
    newWS = if isError then Model.addConsoleMessages ws errors else Model.setMode (Model.setGraph ws g) Model.View
handleEditEvent ws (B.VtyEvent (V.EvKey V.KUp [V.MShift])) = B.continue (Model.Work (Model.moveEditListCursorUp ws))
handleEditEvent ws (B.VtyEvent (V.EvKey V.KDown [V.MShift])) = B.continue (Model.Work (Model.moveEditListCursorDown ws))
handleEditEvent ws (B.VtyEvent (V.EvKey (V.KChar 'f') [])) = B.continue (Model.Work (Model.setCellToSelectedEditListCell ws))
handleEditEvent ws (B.VtyEvent (V.EvKey (V.KChar 'r') [])) = B.continue (Model.Work (Model.setCellToRotate ws))
handleEditEvent ws (B.VtyEvent (V.EvKey (V.KChar 'd') [])) = B.continue (Model.Work (Model.setCellToEmpty ws))
handleEditEvent ws (B.VtyEvent (V.EvKey (V.KChar 'v') [])) = B.continue (Model.Work (Model.setBoardToPrev ws))
handleEditEvent ws _ = B.continue (Model.Work ws)

handleRunEvent :: Model.WorkState -> B.BrickEvent String Name -> B.EventM Name (Next Model.AppState)
handleRunEvent ws (B.VtyEvent (V.EvKey (V.KChar '1') [])) = B.continue (Model.Work (Model.setMode ws Model.View))
handleRunEvent ws _ = B.continue (Model.Work ws)