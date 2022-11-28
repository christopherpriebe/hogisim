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

getContent :: Cell -> CellContent
getContent c = content c

setContent :: Cell -> CellContent -> Cell
setContent (C { content = _, coordinate = coord }) cont = C { content = cont, coordinate = coord }

nextContentForward :: Cell -> Cell
nextContentForward c = setContent c (nextContentForward' (getContent c))

nextContentForward' :: CellContent -> CellContent
nextContentForward' Empty = LowSource
nextContentForward' LowSource = HighSource
nextContentForward' HighSource = UnknownOutput
nextContentForward' UnknownOutput = HorizontalPathNoCross
nextContentForward' HorizontalPathNoCross = VerticalPathNoCross
nextContentForward' _ = Empty

nextContentBackward :: Cell -> Cell
nextContentBackward c = setContent c (nextContentBackward' (getContent c))

nextContentBackward' :: CellContent -> CellContent
nextContentBackward' Empty = Empty
nextContentBackward' LowSource = Empty
nextContentBackward' HighSource = LowSource
nextContentBackward' UnknownOutput = HighSource
nextContentBackward' HorizontalPathNoCross = UnknownOutput
nextContentBackward' _ = Empty

instance Show Cell where
  show c = toString c

toString :: Cell -> String
toString c = toString' (getContent c)

toString' :: CellContent -> String
toString' Empty = "   "
toString' LowSource = "(0)"
toString' HighSource = "(1)"
toString' UnknownOutput = "(?)"
toString' LowOutput = "(0)"
toString' HighOutput = "(1)"
toString' HorizontalPathNoCross = "───"
toString' VerticalPathNoCross = " │ "