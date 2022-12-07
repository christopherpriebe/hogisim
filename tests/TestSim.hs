module TestSim where

import Data.Matrix as M
import Test.HUnit

import Model
import Model.Cell
import Sim
import Data.Either


toBoard :: [[CellContent]] -> Board
toBoard ll = M.matrix size size (\(y, x) -> C { content = ll !! (y - 1) !! (x - 1), coordinate = (y, x) })
  where
    size = Prelude.length ll

--ALL BOARDS SQUARES
directInOut = [ [HighSource, UnknownOutput]
              , [Empty, Empty]
              ]

pathInOut = [ [Empty, Empty, Empty]
            , [HighSource, HorizontalPath, UnknownOutput]
            , [Empty, Empty, Empty]
            ]

andGate = [ [Empty, Empty, Empty, Empty, Empty]
          , [Empty, HighSource, HorizontalANDInputLR, Empty, Empty]
          , [Empty, Empty, HorizontalANDOutputLR, UnknownOutput, Empty]
          , [Empty, HighSource, HorizontalANDInputLR, Empty, Empty]
          , [Empty, Empty, Empty, Empty, Empty]
          ]


halfAdder = [ [HighSource, HorizontalInverterLR, HorizontalPath, HorizontalDownPath, HorizontalANDInputLR, Empty, Empty]
            , [Empty, Empty, Empty, VerticalPath, HorizontalANDOutputLR, UnknownOutput, Empty]
            , [LowSource, HorizontalInverterLR, HorizontalDownPath, PathCross, HorizontalANDInputLR, Empty, Empty]
            , [Empty, Empty, VerticalPath, VerticalPath, Empty, Empty, Empty]
            , [Empty, Empty, VerticalPath, QuadrantOnePath, HorizontalXORInputLR, Empty, Empty]
            , [Empty, Empty, VerticalPath, Empty, HorizontalXOROutputLR, UnknownOutput, Empty]
            , [Empty, Empty, QuadrantOnePath, HorizontalPath, HorizontalXORInputLR, Empty, Empty]
            ]

simpleDisconnect = [ [Empty, Empty, Empty, Empty, Empty]
                   , [Empty, Empty, Empty, Empty, Empty]
                   , [Empty, Empty, HorizontalPath, UnknownOutput, Empty]
                   , [Empty, Empty, Empty, Empty, Empty]
                   , [Empty, Empty, Empty, Empty, Empty]
                   ]

simpleCycle = [ [Empty, Empty, Empty, Empty, Empty]
              , [HighSource, HorizontalDownPath, QuadrantThreePath, Empty, Empty]
              , [Empty, QuadrantOnePath, HorizontalUpPath, UnknownOutput, Empty]
              , [Empty, Empty, Empty, Empty, Empty]
              , [Empty, Empty, Empty, Empty, Empty]
              ]

testMoveTo = TestList
  [ TestCase (assertEqual "Moving up" (moveTo s b DirUp) u)
  , TestCase (assertEqual "Moving right" (moveTo s b DirRight) r)
  , TestCase (assertEqual "Moving down" (moveTo s b DirDown) d)
  , TestCase (assertEqual "Moving left" (moveTo s b DirLeft) l)
  ]
  where 
    b = toBoard directInOut
    s = (3, 3)
    u = Right (2, 3)
    r = Right (3, 4)
    d = Right (4, 3)
    l = Right (3, 2)

testMoveAway = TestList
  [ TestCase (assertEqual "Moving away from up" (moveAway s b DirUp) d)
  , TestCase (assertEqual "Moving away from right" (moveAway s b DirRight) l)
  , TestCase (assertEqual "Moving away from down" (moveAway s b DirDown) u)
  , TestCase (assertEqual "Moving away from left" (moveAway s b DirLeft) r)
  ]
  where 
    b = toBoard directInOut
    s = (3, 3)
    u = Right (2, 3)
    r = Right (3, 4)
    d = Right (4, 3)
    l = Right (3, 2)

testMoveToThrowsOnBoundaryError = TestList
  [ TestCase (assertEqual "Moving up" (moveTo s b DirUp) e)
  , TestCase (assertEqual "Moving right" (moveTo s b DirRight) e)
  , TestCase (assertEqual "Moving down" (moveTo s b DirDown) e)
  , TestCase (assertEqual "Moving left" (moveTo s b DirLeft) e)
  ]
  where 
    b = toBoard [[Empty]]
    s = (1, 1)
    e = Left [(s, disconnectErrorMsg)]


testSolveDirectInputOutput = TestLabel "An output directly next to a high source" (TestCase (fromRight False (solveCell b (1, 2)) @=? True))
  where b = toBoard directInOut

testSolvePathInputOutput = TestLabel "An output connected to a high source" (TestCase (fromRight False (solveCell b (2, 3)) @=? True))
  where b = toBoard pathInOut

testSolveAnd = TestLabel "An output from an AND of two high sources" (TestCase (fromRight False (solveCell b (3, 4)) @=? True))
  where b = toBoard andGate

testSolveHalfAdder = TestList 
  [ TestLabel "The carry output of a half-adder" (TestCase (fromRight True (solveCell b (2, 6)) @=? False))
  , TestLabel "The sum output of a half-adder" (TestCase (fromRight False (solveCell b (6, 6)) @=? True))
  ]
  where b = toBoard halfAdder

testSolveSimpleDisconnectThrows = TestCase (isLeft (solveCell b (3, 4)) @? "Disconnect in circuit")
  where b = toBoard simpleDisconnect

testSolveSimpleCycleThrows = TestCase (isLeft (solveCell b (3, 4)) @? "Cycle in circuit")
  where b = toBoard simpleCycle
