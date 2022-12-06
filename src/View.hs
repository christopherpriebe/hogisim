module View where

import Brick as B
import Brick.Widgets.Border as BWB
import Brick.Widgets.List as BWL
import Graphics.Vty as V
import Data.Matrix as M --TODO: Remove ASAP
import Data.Vector --TODO: Remove ASAP

import Model
import Model.Cell --Add rename
import Types

--TODO: Implement way to pass argument from console to only render using ASCII for certain terminal compatability
view :: AppState -> [B.Widget Name]
view s = [view' s]

view' :: AppState -> B.Widget Name
view' (Menu ms) = case ms of
  New -> BWB.border ((B.withAttr select menuNewWidget) <=> menuLoadWidget <=> menuExitWidget)
  Load -> BWB.border (menuNewWidget <=> (B.withAttr select menuLoadWidget) <=> menuExitWidget)
  Exit -> BWB.border (menuNewWidget <=> menuLoadWidget <=> (B.withAttr select menuExitWidget))
  where
    menuNewWidget = B.str "New Workspace"
    menuLoadWidget = B.str "Load Workspace"
    menuExitWidget = B.str "Exit"
view' (Work ws) = case (mode ws) of
  View -> vboard <=> (B.str "View") <=> vcontr <+> (BWB.border (B.str "STATS HERE"))
  Edit -> vboard <=> (B.str "Edit") <=> econtr <+> (B.hLimit 30 (B.vLimit (boardSize + 2) (viewEditList (editList ws))))
  Run  -> vboard <=> (B.str "Run") <=> rcontr <+> (BWB.border (B.str "STATS HERE"))
  where
    vboard = viewBoard (board ws) (cursorPos ws)
    vcontr = (B.str "Controls:") <=> (B.str "Exit to Menu - \"Esc.\"") <=> (B.str "Switch to \"Edit\" - \"2\"") <=> (B.str "Switch to \"Run\" - \"3\"") <=> (B.str "Move Cursor - (Up | Down | Left | Right) Arrow") <=> (B.str "Save Current View - \"s\" (File will be saved as \"out.hgs\")")
    econtr = (B.str "Controls:") <=> (B.str "Exit to Menu - \"Esc.\"") <=> (B.str "Switch to \"View\" - \"1\"") <=> (B.str "Move Cursor - (Up | Down | Left | Right) Arrow") <=> (B.str "Change Selection - Shift+(Up Arrow | Down Arrow)") <=> (B.str "Place Selected Cell - \"f\"") <=> (B.str "Rotate Selected Cell - \"r\"")
    rcontr = (B.str "Controls:") <=> (B.str "Exit to Menu - \"Esc.\"") <=> (B.str "Switch to \"View\" - \"1\"") <=> (B.str "Move Cursor - (Up | Down | Left | Right) Arrow")

viewBoard :: Board -> Coordinate -> Widget Name
viewBoard b cp = (BWB.border (viewBoardNoBorder b cp))

-- TODO: Modify these functions so Data.Vector is no longer needed
viewBoardNoBorder :: Board -> Coordinate -> Widget Name
viewBoardNoBorder b cp = Prelude.foldl (\acc i -> acc <=> (Data.Vector.foldl1 (<+>) (M.getRow i wb))) (Data.Vector.foldl1 (<+>) (M.getRow 1 wb)) [2..boardSize] --TODO: Fix potential bug with board sizes of 0
  where
    wb = (toWidgetBoard b cp)

toWidgetBoard :: Board -> Coordinate -> M.Matrix (Widget Name)
toWidgetBoard b cp = fmap (\c -> if (coordinate c) == cp then B.withAttr select (B.str (toString c)) else B.str (toString c)) b
--End TODO

viewEditList :: BWL.GenericList String Vector (CellContent, Name) -> Widget Name
viewEditList el = BWB.border (BWL.renderList (\b tup -> if b then B.withAttr select (viewCellContentAndName tup) else viewCellContentAndName tup) False el)

viewCellContentAndName :: (CellContent, Name) -> Widget Name
viewCellContentAndName (cc, n) = (B.str (toString' cc)) <+> (B.str " - ") <+> (B.str n)

select :: B.AttrName
select = B.attrName "select"

attributeMap :: B.AttrMap
attributeMap = B.attrMap V.defAttr [(select, V.withStyle (B.bg V.white) V.bold)]