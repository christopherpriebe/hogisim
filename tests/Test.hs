module Main where

import Test.HUnit

import Data.Vector as V
import Data.Matrix as M

import Types
import Model
import Model.Cell
import Sim

main :: IO ()
main = runTestTTAndExit tests

tests = TestList
  [ TestLabel "emptyTransformEmptyBoard" testTransformEmptyBoard
  ]

testTransformEmptyBoard = TestCase (assertEqual "transform w/ Empty Board Failed" (transform b) [])
  where
    b = toBoard empty5x5

toBoard :: [[CellContent]] -> Board
toBoard ll = M.matrix size size (\(y, x) -> C { content = (ll!!y)!!x, coordinate = (y + 1, x + 1) })
  where
    size = Prelude.length ll

--ALL BOARDS SQUARES
empty5x5 = [ [Empty, Empty, Empty, Empty, Empty]
           , [Empty, Empty, Empty, Empty, Empty]
           , [Empty, Empty, Empty, Empty, Empty]
           , [Empty, Empty, Empty, Empty, Empty]
           , [Empty, Empty, Empty, Empty, Empty]
           ]