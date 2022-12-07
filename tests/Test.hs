module Main where

import Test.HUnit

import TestSim

main :: IO ()
main = runTestTTAndExit tests

tests = TestList
  [ testMoveTo
  , testMoveAway
  , testSolveDirectInputOutput
  , testSolvePathInputOutput
  , testSolveAnd
  , testSolveHalfAdder
  , testSolveSimpleDisconnectThrows
  , testSolveSimpleCycleThrows
  ]
