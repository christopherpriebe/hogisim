module View where

import Brick as B
import Brick.Widgets.Border as BWB
import Graphics.Vty as V

import Model
import Types

view :: AppState -> [B.Widget Name]
view s = [view' s]

view' :: AppState -> B.Widget Name
--TODO: Find a more succinct way to represent menu strings and selected menu options
view' (Menu New) = BWB.border ((B.withAttr menuSelectAttr (menuOptions!!0)) <=> (menuOptions!!1) <=> (menuOptions!!2))
view' (Menu Load) = BWB.border ((menuOptions!!0) <=> (B.withAttr menuSelectAttr (menuOptions!!1)) <=> (menuOptions!!2))
view' (Menu Exit) = BWB.border ((menuOptions!!0) <=> (menuOptions!!1) <=> (B.withAttr menuSelectAttr (menuOptions!!2)))
--End TODO

view' (Work ws) = BWB.border (B.str (toString (board ws)))

menuOptions :: [B.Widget Name]
menuOptions = [B.str "New Workspace", B.str "Load Workspace", B.str "Exit"]

menuSelectAttr :: B.AttrName
menuSelectAttr = B.attrName "menuSelectAttr"

attributeMap :: B.AttrMap
attributeMap = B.attrMap V.defAttr [(menuSelectAttr, V.withStyle (B.fg V.red) V.bold)]