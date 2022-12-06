module Sim where

import Data.Either
import Data.List
import Data.Matrix as M

import Model.Cell as C
import qualified Types as T


type NodeError = ((Int, Int), String)
type UnaryFunc = Bool -> Bool
type BinaryFunc = Bool -> Bool -> Bool


disconnectErrorMsg = "Disconnect error"
cycleDetectedErrorMsg = "Cycle"
multipleInputsError = "Multiple inputs"


data Node
    = Input Bool
    | Direct UnaryFunc Node
    | Gate BinaryFunc Node Node


solve :: Node -> Bool
solve (Input b) = b
solve (Direct f n) = f (solve n)
solve (Gate f n1 n2) = f (solve n1) (solve n2)


data RecursionData = RD
  { visited :: [T.Coordinate]
  , fromDir :: Dir
  }


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


transformCell :: C.Cell -> M.Matrix C.Cell -> RecursionData -> Either NodeError Node
transformCell (C.C C.LowSource _) = transformInput False
transformCell (C.C C.HighSource _) = transformInput True
transformCell (C.C C.HorizontalPath coord) = transformPath [DirLeft, DirRight] coord
transformCell (C.C C.VerticalRightPath coord) = transformPath [DirUp, DirRight, DirDown] coord
transformCell (C.C C.HorizontalAND coord) = transformGateBody andGates coord
transformCell (C.C C.HorizontalANDInputLR coord) = transformGateInput DirLeft (&&) andGates coord
transformCell (C.C C.HorizontalANDInputRL coord) = transformGateInput DirRight (&&) andGates coord
transformCell (C.C C.HorizontalANDOutputLR coord) = transformGateOutput DirRight (&&) andGates coord
transformCell (C.C C.HorizontalANDOutputRL coord) = transformGateOutput DirLeft (&&) andGates coord
-- TODO: Make sure all cases covered
transformCell _ = transformInput False


transformInput :: Bool -> M.Matrix C.Cell -> RecursionData -> Either NodeError Node
transformInput b _ _ = return (Input b)


transformPath :: [Dir] -> T.Coordinate -> M.Matrix C.Cell -> RecursionData -> Either NodeError Node
transformPath allowedDirs coord m rd
    | coord `elem` visited rd = Left (coord, cycleDetectedErrorMsg)
    | fromDir rd `notElem` allowedDirs = Left (coord, disconnectErrorMsg)
    | otherwise = do
        let depDirs = delete (fromDir rd) allowedDirs
        let depNodes = [transformCell (m ! (coord `moveTo` depDir)) m (RD v (flipDir depDir)) | depDir <- depDirs]
        let numValidNodes = numRight depNodes
        if numValidNodes == 0 
            then head depNodes
        else if numValidNodes > 1 
            then Left (coord, multipleInputsError)
        else head (filter isRight depNodes)
        where 
            v = coord:visited rd


transformGateBody :: [C.CellContent] -> T.Coordinate -> M.Matrix C.Cell -> RecursionData -> Either NodeError Node
transformGateBody gates coord m rd
    | fromDir rd == DirLeft || fromDir rd == DirRight = Left (coord, disconnectErrorMsg)
    | otherwise = do
        if getContent depCell `elem` gates then do
            depNode <- transformCell depCell m (RD v (fromDir rd))
            return (Direct id depNode)
        else Left (coord, disconnectErrorMsg)
        where
            depCell = m ! (coord `moveAway` fromDir rd)
            v = coord:visited rd


transformUnaryGate :: Dir -> UnaryFunc -> T.Coordinate -> M.Matrix C.Cell -> RecursionData -> Either NodeError Node
transformUnaryGate outputDir f coord m rd
    | fromDir rd /= outputDir = Left (coord, disconnectErrorMsg)
    | otherwise = do
        depNode <- transformCell (m ! (coord `moveAway` fromDir rd)) m (RD v (fromDir rd))
        return (Direct f depNode)
        where
            v = coord:visited rd


transformGateInput :: Dir -> BinaryFunc -> [C.CellContent] -> T.Coordinate -> M.Matrix C.Cell -> RecursionData -> Either NodeError Node
transformGateInput inputDir f gates coord m rd
    | fromDir rd == DirLeft || fromDir rd == DirRight = Left (coord, disconnectErrorMsg)
    | otherwise = do
        inputNode <- transformCell (m ! (coord `moveTo` inputDir)) m (RD v (flipDir inputDir))
        if getContent depCell `elem` gates then do
            prevNode <- transformCell depCell m (RD v (fromDir rd))
            return (Gate f inputNode prevNode)
        else return (Direct id inputNode)
        where 
            depCell = m ! (coord `moveAway` fromDir rd)
            v = coord:visited rd


transformGateOutput :: Dir -> BinaryFunc -> [C.CellContent] -> T.Coordinate -> M.Matrix C.Cell -> RecursionData -> Either NodeError Node
transformGateOutput outputDir f gates coord m rd
    | fromDir rd /= outputDir = Left (coord, disconnectErrorMsg)
    | otherwise = do
        if upHasGate && downHasGate then do
            depNodeUp <- transformCell depCellUp m (RD v DirDown)
            depNodeDown <- transformCell depCellDown m (RD v DirUp)
            return (Gate f depNodeUp depNodeDown)
        else if upHasGate then do
            depNodeUp <- transformCell depCellUp m (RD v DirDown)
            return (Direct id depNodeUp)
        else if downHasGate then do
            depNodeDown <- transformCell depCellDown m (RD v DirUp)
            return (Direct id depNodeDown)
        else Left (coord, disconnectErrorMsg)
        where
            depCellUp = m ! (coord `moveTo` DirUp)
            depCellDown = m ! (coord `moveTo` DirDown)
            upHasGate = getContent depCellUp `elem` gates
            downHasGate = getContent depCellDown `elem` gates
            v = coord:visited rd


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


flipDir :: Dir -> Dir
flipDir DirUp = DirDown
flipDir DirRight = DirLeft
flipDir DirDown = DirUp
flipDir DirLeft = DirRight
flipDir DirNone = DirNone


numRight :: [Either a b] -> Int
numRight = length . filter isRight
