module Main where

import Brick as B
import Data.Maybe

import Control
import Model
import Types
import View

main :: IO ()
main = do
  finalState <- B.defaultMain app initialState
  print "Hogism has successfully exited."
  where
    initialState = Model.initAppState

app :: App AppState String Name
app = B.App
  { appDraw         = view
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const attributeMap
  }