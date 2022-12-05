module Sim where

import Data.Matrix as M

import Model.Cell as C
import qualified Types as T
import Data.Either

type ErrorType = ((Int, Int), String)

disconnectErrorMsg = "Disconnect error"
cycleDetectedErrorMsg = "Cycle"
multipleInputsError = "Multiple inputs"


data Node
    = Input Bool
    | Direct (Bool -> Bool) Node
    | Gate (Bool -> Bool-> Bool) [Node]


data Dir
    = DirUp
    | DirRight
    | DirDown
    | DirLeft
    | DirNone
    deriving (Eq)

andGates = [
    C.HorizontalAND, 
    C.HorizontalANDInputLR, 
    C.HorizontalANDInputRL, 
    C.HorizontalANDOutputLR, 
    C.HorizontalANDOutputRL
    ]

transform :: M.Matrix C.Cell -> [Node]
transform _ = []


transformCell :: M.Matrix C.Cell -> C.Cell -> [T.Coordinate] -> Dir -> Either ErrorType Node

transformCell _ (C.C C.LowSource _)  _ _ = return (Input False)

transformCell _ (C.C C.HighSource _) _ _ = return (Input True)

transformCell m (C.C C.HorizontalPath coord) path fromDir
    | fromDir == DirUp || fromDir == DirDown = Left (coord, disconnectErrorMsg)
    | otherwise = do
        prevNode <- transformCell m (m ! (coord `moveAway` fromDir)) (coord:path) fromDir
        return (Direct id prevNode)

transformCell m (C.C C.VerticalRightPath coord) path fromDir
    | coord `elem` path = Left (coord, cycleDetectedErrorMsg)
    | fromDir == DirLeft = Left (coord, disconnectErrorMsg)
    | otherwise = do
        let prevDirUp = transformCell m (m ! (coord `moveTo` DirUp)) (coord:path) DirDown
        let prevDirDown = transformCell m (m ! (coord `moveTo` DirDown)) (coord:path) DirUp
        let prevDirRight = transformCell m (m ! (coord `moveTo` DirRight)) (coord:path) DirLeft
        let paths = [prevDirUp, prevDirDown, prevDirRight]
        let numValidPaths = numRight paths
        if numValidPaths == 0 then case fromDir of
          DirUp -> prevDirDown
          DirRight -> prevDirUp
          DirDown -> prevDirRight
          _ -> prevDirUp
        else if numValidPaths > 1 then Left (coord, multipleInputsError)
        else head (filter isRight paths)

transformCell m (C.C C.HorizontalAND coord) path fromDir
    | fromDir == DirLeft || fromDir == DirRight = Left (coord, disconnectErrorMsg)
    | otherwise = do
        let prevCell = m ! (coord `moveAway` fromDir)
        if getContent prevCell `elem` andGates then do
            prevGate <- transformCell m prevCell (coord:path) fromDir
            return (Direct id prevGate)
        else Left (coord, disconnectErrorMsg)
transformCell m (C.C C.HorizontalANDInputLR coord) path fromDir
    | fromDir == DirLeft || fromDir == DirRight = Left (coord, disconnectErrorMsg)
    | otherwise = do
        inputNode <- transformCell m (m ! (coord `moveTo` DirLeft)) (coord:path) DirRight
        let prevCell = m ! (coord `moveAway` fromDir)
        if getContent prevCell `elem` andGates then do
            prevGate <- transformCell m prevCell (coord:path) fromDir
            return (Gate (&&) [inputNode, prevGate])
        else return (Direct id inputNode)
transformCell m (C.C C.HorizontalANDInputRL coord) path fromDir
    | fromDir == DirLeft || fromDir == DirRight = Left (coord, disconnectErrorMsg)
    | otherwise = do
        inputNode <- transformCell m (m ! (coord `moveTo` DirRight)) (coord:path) DirLeft
        let prevCell = m ! (coord `moveAway` fromDir)
        if getContent prevCell `elem` andGates then do
            prevGate <- transformCell m prevCell (coord:path) fromDir
            return (Gate (&&) [inputNode, prevGate])
        else return (Direct id inputNode)
transformCell m (C.C C.HorizontalANDOutputLR coord) path fromDir
    | fromDir /= DirRight = Left (coord, disconnectErrorMsg)
    | otherwise = do
        let prevCellUp = m ! (coord `moveTo` DirUp)
        let prevCellDown = m ! (coord `moveTo` DirDown)
        let upHasGate = getContent prevCellUp `elem` andGates
        let downHasGate = getContent prevCellDown `elem` andGates
        if upHasGate && downHasGate then do
            prevNodeUp <- transformCell m prevCellUp (coord:path) DirDown
            prevNodeDown <- transformCell m prevCellDown (coord:path) DirDown
            return (Gate (&&) [prevNodeUp, prevNodeDown])
        else if upHasGate then do
            prevNodeUp <- transformCell m prevCellUp (coord:path) DirDown
            return (Direct id prevNodeUp)
        else if downHasGate then do
            prevNodeDown <- transformCell m prevCellDown (coord:path) DirUp
            return (Direct id prevNodeDown)
        else Left (coord, disconnectErrorMsg)


-- TODO: Make sure all cases covered
transformCell _ _ _ _ = return (Input False)

moveTo :: T.Coordinate -> Dir -> T.Coordinate
moveTo (x, y) DirUp = (x, y + 1)
moveTo (x, y) DirRight = (x + 1, y)
moveTo (x, y) DirDown = (x, y - 1)
moveTo (x, y) DirLeft = (x - 1, y)
moveTo (x, y) DirNone = (x, y)

moveAway :: T.Coordinate -> Dir -> T.Coordinate
moveAway (x, y) DirUp = (x, y - 1)
moveAway (x, y) DirRight = (x - 1, y)
moveAway (x, y) DirDown = (x, y + 1)
moveAway (x, y) DirLeft = (x + 1, y)
moveAway (x, y) DirNone = (x, y)

solve :: Node -> Bool
solve (Input b) = b
solve (Direct f n) = f (solve n)
solve (Gate f ns) = foldr1 f inputs
    where inputs = [solve n | n <- ns]


numRight :: [Either a b] -> Int
numRight = length . filter isRight

-- >>> g = Gate (&&) [(Gate (||) [Input True, Input False]), Direct (not) (Input True), Input True]
-- >>> solve g
-- False

