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
directInOut = [ [Empty, Empty, Empty, Empty, Empty]
              , [Empty, Empty, Empty, Empty, Empty]
              , [Empty, Empty, HighSource, UnknownOutput, Empty]
              , [Empty, Empty, Empty, Empty, Empty]
              , [Empty, Empty, Empty, Empty, Empty]
              ]

pathInOut = [ [Empty, Empty, Empty, Empty, Empty]
            , [Empty, Empty, Empty, Empty, Empty]
            , [Empty, HighSource, HorizontalPath, UnknownOutput, Empty]
            , [Empty, Empty, Empty, Empty, Empty]
            , [Empty, Empty, Empty, Empty, Empty]
            ]

andGate = [ [Empty, Empty, Empty, Empty, Empty]
          , [Empty, HighSource, HorizontalANDInputLR, Empty, Empty]
          , [Empty, Empty, HorizontalANDOutputLR, UnknownOutput, Empty]
          , [Empty, HighSource, HorizontalANDInputLR, Empty, Empty]
          , [Empty, Empty, Empty, Empty, Empty]
          ]


halfAdder = [ [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
            , [Empty, HighSource, HorizontalInverterLR, HorizontalPath, HorizontalDownPath, HorizontalANDInputLR, Empty, Empty, Empty]
            , [Empty, Empty, Empty, Empty, VerticalPath, HorizontalANDOutputLR, UnknownOutput, Empty, Empty]
            , [Empty, LowSource, HorizontalInverterLR, HorizontalDownPath, PathCross, HorizontalANDInputLR, Empty, Empty, Empty]
            , [Empty, Empty, Empty, VerticalPath, VerticalPath, Empty, Empty, Empty, Empty]
            , [Empty, Empty, Empty, VerticalPath, QuadrantOnePath, HorizontalXORInputLR, Empty, Empty, Empty]
            , [Empty, Empty, Empty, VerticalPath, Empty, HorizontalXOROutputLR, UnknownOutput, Empty, Empty]
            , [Empty, Empty, Empty, QuadrantOnePath, HorizontalPath, HorizontalXORInputLR, Empty, Empty, Empty]
            , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
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
  [ TestCase (assertEqual "Moving up" (s `moveTo` DirUp) u)
  , TestCase (assertEqual "Moving right" (s `moveTo` DirRight) r)
  , TestCase (assertEqual "Moving down" (s `moveTo` DirDown) d)
  , TestCase (assertEqual "Moving left" (s `moveTo` DirLeft) l)
  ]
  where 
    s = (3, 3)
    u = (2, 3)
    r = (3, 4)
    d = (4, 3)
    l = (3, 2)

testMoveAway = TestList
  [ TestCase (assertEqual "Moving away from up" (s `moveAway` DirUp) d)
  , TestCase (assertEqual "Moving away from right" (s `moveAway` DirRight) l)
  , TestCase (assertEqual "Moving away from down" (s `moveAway` DirDown) u)
  , TestCase (assertEqual "Moving away from left" (s `moveAway` DirLeft) r)
  ]
  where 
    s = (3, 3)
    u = (2, 3)
    r = (3, 4)
    d = (4, 3)
    l = (3, 2)


testSolveDirectInputOutput = TestLabel "An output directly next to a high source" (TestCase (fromRight False (solveCell b (3, 4)) @=? True))
  where b = toBoard directInOut

testSolvePathInputOutput = TestLabel "An output connected to a high source" (TestCase (fromRight False (solveCell b (3, 4)) @=? True))
  where b = toBoard pathInOut

testSolveAnd = TestLabel "An output from an AND of two high sources" (TestCase (fromRight False (solveCell b (3, 4)) @=? True))
  where b = toBoard andGate

testSolveHalfAdder = TestList 
  [ TestLabel "The carry output of a half-adder" (TestCase (fromRight True (solveCell b (3, 7)) @=? False))
  , TestLabel "The sum output of a half-adder" (TestCase (fromRight False (solveCell b (7, 7)) @=? True))
  ]
  where b = toBoard halfAdder

testSolveSimpleDisconnectThrows = TestCase (isLeft (solveCell b (3, 4)) @? "Disconnect in circuit")
  where b = toBoard simpleDisconnect

testSolveSimpleCycleThrows = TestCase (isLeft (solveCell b (3, 4)) @? "Cycle in circuit")
  where b = toBoard simpleCycle
