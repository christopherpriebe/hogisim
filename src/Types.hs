module Types where

type Name = String
type Coordinate = (Int, Int)

boardSize :: Int
boardSize = 30 --TODO: Implement way to pass board size (within reason) from command line

consoleSize :: Int
consoleSize = 5