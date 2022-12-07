module Main where

import Test.HUnit

import TestSim

main :: IO ()
main = runTestTTAndExit tests

tests = TestList
  [ testMoveTo
  , testMoveAway
  , testMoveToThrowsOnBoundaryError
  , testSolveDirectInputOutput
  , testSolvePathInputOutput
  , testSolveAnd
  , testSolveMultiNor
  , testSolveHalfAdder
  , testSolveSimpleDisconnectThrows
  , testSolveSimpleCycleThrows
  ]
