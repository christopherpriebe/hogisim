module Model.Cell where

import Types

data Cell = C
  { content :: CellContent
  , coordinate :: Coordinate
  }

data CellContent
  = Empty
  | LowSource
  | HighSource
  | UnknownOutput
  | LowOutput
  | HighOutput
  | HorizontalPathNoCross
  | VerticalPathNoCross --TODO: Add the remaining possible states
  deriving (Eq)

instance Show Cell where
  show c = toString c

toString :: Cell -> String
toString c = toString' (content c)

toString' :: CellContent -> String
toString' Empty = "   "
toString' LowSource = "(0)"
toString' HighSource = "(1)"
toString' UnknownOutput = "(?)"
toString' LowOutput = "(0)"
toString' HighOutput = "(1)"
toString' HorizontalPathNoCross = "───"
toString' VerticalPathNoCross = " │ "