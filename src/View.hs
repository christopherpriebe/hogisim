module View where

import Brick as B
import Brick.Widgets.Border as BWB
import Brick.Widgets.List as BWL
import Graphics.Vty as V
import Data.Matrix as M
import Data.Vector as Vec

import Model
import Model.Cell --TODO: add rename as C
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
view' (Work ws) = case (getMode ws) of
  View -> (vboard <=> (B.str "View") <=> vcontr) <+> (vboardRightBox <=> vconsole)
  Edit -> (vboard <=> (B.str "Edit") <=> econtr) <+> (eboardRightBox <=> vconsole)
  Run  -> (vboard <=> (B.str "Run") <=> rcontr) <+> (rboardRightBox <=> vconsole)
  where
    vboard = viewBoard (board ws) (cursorPos ws)
    vcontr = (B.str "Controls:") <=> (B.str "Exit to Menu - \"Esc.\"") <=> (B.str "Switch to \"Edit\" - \"2\"") <=> (B.str "Switch to \"Run\" - \"3\"") <=> (B.str "Move Cursor - (Up | Down | Left | Right) Arrow") <=> (B.str "Save Current View - \"s\" (File will be saved as \"out.hgs\")")
    econtr = (B.str "Controls:") <=> (B.str "Exit to Menu - \"Esc.\"") <=> (B.str "Switch to \"View\" - \"1\"") <=> (B.str "Move Cursor - (Up | Down | Left | Right) Arrow") <=> (B.str "Change Selection - Shift+(Up Arrow | Down Arrow)") <=> (B.str "Place Selected Cell - \"f\"") <=> (B.str "Clear Cell - \"d\"") <=> (B.str "Rotate Selected Cell - \"r\"")
    rcontr = (B.str "Controls:") <=> (B.str "Exit to Menu - \"Esc.\"") <=> (B.str "Switch to \"View\" - \"1\"") <=> (B.str "Move Cursor - (Up | Down | Left | Right) Arrow")
    vboardRightBoxNoPad = B.str "stats here"
    vboardRightBox = padRightBox vboardRightBoxNoPad
    eboardRightBoxNoPad = viewEditList (editList ws)
    eboardRightBox = padRightBox eboardRightBoxNoPad
    rboardRightBoxNoPad = B.str "stats here"
    rboardRightBox = padRightBox rboardRightBoxNoPad
    vconsole = padConsole (B.vBox (Vec.toList (Vec.map (\s -> B.str s) (Model.getConsole ws))))

padRightBox :: B.Widget Name -> B.Widget Name
padRightBox w = BWB.border (B.hLimit 50 (B.vLimit boardSize (B.padRight B.Max (B.padBottom B.Max w))))

padConsole :: B.Widget Name -> B.Widget Name
padConsole w = BWB.border (B.hLimit 50 (B.vLimit 5 (B.padRight B.Max (B.padTop B.Max w))))

viewBoard :: Board -> Coordinate -> Widget Name
viewBoard b cp = (BWB.border (viewBoardNoBorder b cp))

-- TODO: Modify these functions so Data.Vector is no longer needed
viewBoardNoBorder :: Board -> Coordinate -> Widget Name
viewBoardNoBorder b cp = Prelude.foldl (\acc i -> acc <=> (Vec.foldl1 (<+>) (M.getRow i wb))) (Vec.foldl1 (<+>) (M.getRow 1 wb)) [2..boardSize] --TODO: Fix potential bug with board sizes of 0
  where
    wb = (toWidgetBoard b cp)

toWidgetBoard :: Board -> Coordinate -> M.Matrix (Widget Name)
toWidgetBoard b cp = fmap (\c -> if (coordinate c) == cp then B.withAttr select (B.str (toString c)) else B.str (toString c)) b
--End TODO

viewEditList :: BWL.GenericList String Vector (CellContent, Name) -> Widget Name
viewEditList el = BWL.renderList (\b tup -> if b then B.withAttr select (viewCellContentAndName tup) else viewCellContentAndName tup) False el

viewCellContentAndName :: (CellContent, Name) -> Widget Name
viewCellContentAndName (cc, n) = (B.str (toString' cc)) <+> (B.str " - ") <+> (B.str n)

select :: B.AttrName
select = B.attrName "select"

attributeMap :: B.AttrMap
attributeMap = B.attrMap V.defAttr [(select, V.withStyle (B.bg V.white) V.bold)]