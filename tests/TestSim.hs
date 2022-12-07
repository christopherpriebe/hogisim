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
empty5x5 = [ [Empty, Empty, Empty, Empty, Empty]
           , [Empty, Empty, Empty, Empty, Empty]
           , [Empty, Empty, Empty, Empty, Empty]
           , [Empty, Empty, Empty, Empty, Empty]
           , [Empty, Empty, Empty, Empty, Empty]
           ]

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


testSolveDirectInputOutput = TestCase (assertEqual "Direct input from high source should be True" True o)
  where
    b = toBoard directInOut
    o = fromRight False (solveCell b (3, 4))

testSolvePathInputOutput = TestCase (assertEqual "An output with a path to a high source should be True" True o)
  where
    b = toBoard pathInOut
    o = fromRight False (solveCell b (3, 4))

testSolveAnd = TestCase (assertEqual "An output from and AND of two high sources should be True" True o)
  where
    b = toBoard andGate
    o = fromRight False (solveCell b (3, 4))

