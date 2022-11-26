module View where

import Brick as B
import Brick.Widgets.Border as BWB
import Graphics.Vty as V
import Data.Vector --TODO: Remove ASAP

import Model
import Model.Cell
import Types

view :: AppState -> [B.Widget Name]
view s = [view' s]

view' :: AppState -> B.Widget Name
--TODO: Find a more succinct way to represent menu strings and selected menu options
view' (Menu New) = BWB.border ((B.withAttr select (menuOptions!!0)) <=> (menuOptions!!1) <=> (menuOptions!!2))
view' (Menu Load) = BWB.border ((menuOptions!!0) <=> (B.withAttr select (menuOptions!!1)) <=> (menuOptions!!2))
view' (Menu Exit) = BWB.border ((menuOptions!!0) <=> (menuOptions!!1) <=> (B.withAttr select (menuOptions!!2)))
--End TODO
--view' (Work ws) = case (mode ws) of
--  View -> (BWB.border (B.str (toString (board ws)))) <+> (BWB.border (B.str "STATS HERE"))
--  Edit -> (BWB.border (B.str (toString (board ws)))) <+> (BWB.border (B.str "GATES HERE"))
--  Run -> (BWB.border (B.str (toString (board ws)))) <+> (BWB.border (B.str "RUN STATS HERE"))
view' (Work ws) = case (mode ws) of
  View -> (BWB.border (viewBoardWithCursor (board ws) (cursorPos ws))) <+> (BWB.border (B.str "STATS HERE")) <=> (B.str "View")
  Edit -> (BWB.border (viewBoardWithCursor (board ws) (cursorPos ws))) <+> (BWB.border (B.str "STATS HERE")) <=> (B.str "Edit")
  Run -> (BWB.border (viewBoardWithCursor (board ws) (cursorPos ws))) <+> (BWB.border (B.str "STATS HERE")) <=> (B.str "Run")

-- TODO: Modify these functions so Data.Vector is no longer needed
viewBoardWithCursor :: Board -> Coordinate -> Widget Name
viewBoardWithCursor b cp = B.vBox (Data.Vector.toList (Data.Vector.map B.hBox (toWidgetBoard b cp)))

toWidgetBoard :: Board -> Coordinate -> Data.Vector.Vector [B.Widget Name]
toWidgetBoard b cp = Data.Vector.map (\r -> Prelude.map (\c -> if (coordinate c) == cp then B.withAttr select (B.str (toString c)) else B.str (toString c)) (Data.Vector.toList r)) b
--End TODO

menuOptions :: [B.Widget Name]
menuOptions = [B.str "New Workspace", B.str "Load Workspace", B.str "Exit"]

select :: B.AttrName
select = B.attrName "select"

attributeMap :: B.AttrMap
attributeMap = B.attrMap V.defAttr [(select, V.withStyle (B.bg V.white) V.bold)]