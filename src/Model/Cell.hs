module Model.Cell where

import Data.Vector as V

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
  | HorizontalPath
  | VerticalPath
  | PathCross
  | VerticalRightPath
  | VerticalLeftPath
  | HorizontalUpPath
  | HorizontalDownPath
  | PathIntersection
  | QuadrantThreePath
  | QuadrantTwoPath
  | QuadrantFourPath
  | QuadrantOnePath
  | HorizontalBufferLR
  | HorizontalBufferRL
  | HorizontalInverterLR
  | HorizontalInverterRL
  | HorizontalORInputLR
  | HorizontalORInputRL
  | HorizontalOROutputLR
  | HorizontalOROutputRL
  | HorizontalOR
  | HorizontalANDInputLR
  | HorizontalANDInputRL
  | HorizontalANDOutputLR
  | HorizontalANDOutputRL
  | HorizontalAND
  | HorizontalXORInputLR
  | HorizontalXORInputRL
  | HorizontalXOROutputLR
  | HorizontalXOROutputRL
  | HorizontalXOR
  | HorizontalNORInputLR
  | HorizontalNORInputRL
  | HorizontalNOROutputLR
  | HorizontalNOROutputRL
  | HorizontalNOR
  | HorizontalNANDInputLR
  | HorizontalNANDInputRL
  | HorizontalNANDOutputLR
  | HorizontalNANDOutputRL
  | HorizontalNAND
  | HorizontalXNORInputLR
  | HorizontalXNORInputRL
  | HorizontalXNOROutputLR
  | HorizontalXNOROutputRL
  | HorizontalXNOR
  deriving (Eq)

getContent :: Cell -> CellContent
getContent c = content c

setContent :: Cell -> CellContent -> Cell
setContent (C { content = _, coordinate = coord }) cont = C { content = cont, coordinate = coord }

rotate :: Cell -> Cell
rotate c = setContent c (rotate' (getContent c))

rotate' :: CellContent -> CellContent
rotate' HorizontalPath = VerticalPath
rotate' VerticalPath = HorizontalPath
rotate' VerticalRightPath = HorizontalDownPath
rotate' HorizontalDownPath = VerticalLeftPath
rotate' VerticalLeftPath = HorizontalUpPath
rotate' HorizontalUpPath = VerticalRightPath
rotate' QuadrantFourPath = QuadrantThreePath
rotate' QuadrantThreePath = QuadrantTwoPath
rotate' QuadrantTwoPath = QuadrantOnePath
rotate' QuadrantOnePath = QuadrantFourPath
rotate' HorizontalBufferLR = HorizontalBufferRL
rotate' HorizontalBufferRL = HorizontalBufferLR
rotate' HorizontalInverterLR = HorizontalInverterRL
rotate' HorizontalInverterRL = HorizontalInverterLR
rotate' HorizontalORInputLR = HorizontalORInputRL
rotate' HorizontalORInputRL = HorizontalORInputLR
rotate' HorizontalOROutputLR = HorizontalOROutputRL
rotate' HorizontalOROutputRL = HorizontalOROutputLR
rotate' HorizontalANDInputLR = HorizontalANDInputRL
rotate' HorizontalANDInputRL = HorizontalANDInputLR
rotate' HorizontalANDOutputLR = HorizontalANDOutputRL
rotate' HorizontalANDOutputRL = HorizontalANDOutputLR
rotate' HorizontalXORInputLR = HorizontalXORInputRL
rotate' HorizontalXORInputRL = HorizontalXORInputLR
rotate' HorizontalXOROutputLR = HorizontalXOROutputRL
rotate' HorizontalXOROutputRL = HorizontalXOROutputLR
rotate' HorizontalNORInputLR = HorizontalNORInputRL
rotate' HorizontalNORInputRL = HorizontalNORInputLR
rotate' HorizontalNOROutputLR = HorizontalNOROutputRL
rotate' HorizontalNOROutputRL = HorizontalNOROutputLR
rotate' HorizontalNANDInputLR = HorizontalNANDInputRL
rotate' HorizontalNANDInputRL = HorizontalNANDInputLR
rotate' HorizontalNANDOutputLR = HorizontalNANDOutputRL
rotate' HorizontalNANDOutputRL = HorizontalNANDOutputLR
rotate' HorizontalXNORInputLR = HorizontalXNORInputRL
rotate' HorizontalXNORInputRL = HorizontalXNORInputLR
rotate' HorizontalXNOROutputLR = HorizontalXNOROutputRL
rotate' HorizontalXNOROutputRL = HorizontalXNOROutputLR
rotate' cont = cont

getCoordinate :: Cell -> Coordinate
getCoordinate c = coordinate c

setCoordinate:: Cell -> Coordinate -> Cell
setCoordinate (C { content = cont, coordinate = _ }) coord = C { content = cont, coordinate = coord }

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
toString' HorizontalPath = "───"
toString' VerticalPath = " │ "
toString' PathCross = "─│─"
toString' PathIntersection = "─┼─"
toString' VerticalRightPath = " ├─"
toString' VerticalLeftPath = "─┤ "
toString' HorizontalUpPath = "─┴─"
toString' HorizontalDownPath = "─┬─"
toString' QuadrantThreePath = "─┐ "
toString' QuadrantTwoPath = "─┘ "
toString' QuadrantFourPath = " ┌─"
toString' QuadrantOnePath = " └─"
toString' HorizontalBufferLR = "┤=┠"
toString' HorizontalBufferRL = "┨=├"
toString' HorizontalInverterLR = "┤¬┠"
toString' HorizontalInverterRL = "┨¬├"
toString' HorizontalORInputLR = "┤⋁│"
toString' HorizontalORInputRL = "│⋁├"
toString' HorizontalOROutputLR = "│⋁┠"
toString' HorizontalOROutputRL = "┨⋁│"
toString' HorizontalOR = "│⋁│"
toString' HorizontalANDInputLR = "┤⋀│"
toString' HorizontalANDInputRL = "│⋀├"
toString' HorizontalANDOutputLR = "│⋀┠"
toString' HorizontalANDOutputRL = "┨⋀│"
toString' HorizontalAND = "│⋀│"
toString' HorizontalXORInputLR = "┤⊻│"
toString' HorizontalXORInputRL = "│⊻├"
toString' HorizontalXOROutputLR = "│⊻┠"
toString' HorizontalXOROutputRL = "┨⊻│"
toString' HorizontalXOR = "│⊻│"
toString' HorizontalNORInputLR = "┤⊽│"
toString' HorizontalNORInputRL = "│⊽├"
toString' HorizontalNOROutputLR = "│⊽┠"
toString' HorizontalNOROutputRL = "┨⊽│"
toString' HorizontalNOR = "│⊽│"
toString' HorizontalNANDInputLR = "┤⊼│"
toString' HorizontalNANDInputRL = "│⊼├"
toString' HorizontalNANDOutputLR = "│⊼┠"
toString' HorizontalNANDOutputRL = "┨⊼│"
toString' HorizontalNAND = "│⊼│"
toString' HorizontalXNORInputLR = "┤↔│"
toString' HorizontalXNORInputRL = "│↔├"
toString' HorizontalXNOROutputLR = "│↔┠"
toString' HorizontalXNOROutputRL = "┨↔│"
toString' HorizontalXNOR = "│↔│"

placeableCellContents :: V.Vector (CellContent, Name)
placeableCellContents = V.fromList l
  where
    l =
      [ (Empty, "Blank Space")
      , (LowSource, "Low Source")
      , (HighSource, "High Source")
      , (UnknownOutput, "Output Node")
      , (HorizontalPath, "Path")
      , (VerticalPath, "Path")
      , (PathCross, "Path w/ Cross")
      , (PathIntersection, "Path w/ Four-Way Intersection")
      , (VerticalRightPath, "Path w/ T-Intersection")
      , (QuadrantFourPath, "Corner Path")
      , (HorizontalBufferLR, "Buffer")
      , (HorizontalInverterLR, "Inverter")
      , (HorizontalORInputLR, "OR Gate Input")
      , (HorizontalOROutputLR, "OR Gate Output")
      , (HorizontalOR, "OR Gate Body")
      , (HorizontalANDInputLR, "AND Gate Input")
      , (HorizontalANDOutputLR, "AND Gate Output")
      , (HorizontalAND, "AND Gate Body")
      , (HorizontalXORInputLR, "XOR Gate Input")
      , (HorizontalXOROutputLR, "XOR Gate Output")
      , (HorizontalXOR, "XOR Gate Body")
      , (HorizontalNORInputLR, "NOR Gate Input")
      , (HorizontalNOROutputLR, "NOR Gate Output")
      , (HorizontalNOR, "NOR Gate Body")
      , (HorizontalNANDInputLR, "NAND Gate Input")
      , (HorizontalNANDOutputLR, "NAND Gate Output")
      , (HorizontalNAND, "NAND Gate Body")
      , (HorizontalXNORInputLR, "XNOR Gate Input")
      , (HorizontalXNOROutputLR, "XNOR Gate Output")
      , (HorizontalXNOR, "XNOR Gate Body")
      ]

toFileEncoding :: CellContent -> FileEncoding
toFileEncoding Empty = ' '
toFileEncoding LowSource = '!'
toFileEncoding HighSource = '"'
toFileEncoding UnknownOutput = '#'
toFileEncoding HorizontalPath = '$'
toFileEncoding VerticalPath = '%'
toFileEncoding PathCross = '&'
toFileEncoding PathIntersection = '\''
toFileEncoding VerticalRightPath = '('
toFileEncoding VerticalLeftPath = ')'
toFileEncoding HorizontalUpPath = '*'
toFileEncoding HorizontalDownPath = '+'
toFileEncoding QuadrantThreePath = ','
toFileEncoding QuadrantTwoPath = '-'
toFileEncoding QuadrantFourPath = '.'
toFileEncoding QuadrantOnePath = '/'
toFileEncoding HorizontalBufferLR = '0'
toFileEncoding HorizontalBufferRL = '1'
toFileEncoding HorizontalInverterLR = '2'
toFileEncoding HorizontalInverterRL = '3'
toFileEncoding HorizontalORInputLR = '4'
toFileEncoding HorizontalORInputRL = '5'
toFileEncoding HorizontalOROutputLR = '6'
toFileEncoding HorizontalOROutputRL = '7'
toFileEncoding HorizontalOR = '8'
toFileEncoding HorizontalANDInputLR = '9'
toFileEncoding HorizontalANDInputRL = ':'
toFileEncoding HorizontalANDOutputLR = ';'
toFileEncoding HorizontalANDOutputRL = '<'
toFileEncoding HorizontalAND = '='
toFileEncoding HorizontalXORInputLR = '>'
toFileEncoding HorizontalXORInputRL = '?'
toFileEncoding HorizontalXOROutputLR = '@'
toFileEncoding HorizontalXOROutputRL = 'A'
toFileEncoding HorizontalXOR = 'B'
toFileEncoding HorizontalNORInputLR = 'C'
toFileEncoding HorizontalNORInputRL = 'D'
toFileEncoding HorizontalNOROutputLR = 'E'
toFileEncoding HorizontalNOROutputRL = 'F'
toFileEncoding HorizontalNOR = 'G'
toFileEncoding HorizontalNANDInputLR = 'H'
toFileEncoding HorizontalNANDInputRL = 'I'
toFileEncoding HorizontalNANDOutputLR = 'J'
toFileEncoding HorizontalNANDOutputRL = 'K'
toFileEncoding HorizontalNAND = 'L'
toFileEncoding HorizontalXNORInputLR = 'M'
toFileEncoding HorizontalXNORInputRL = 'N'
toFileEncoding HorizontalXNOROutputLR = 'O'
toFileEncoding HorizontalXNOROutputRL = 'P'
toFileEncoding HorizontalXNOR = 'Q'
toFileEncoding _ = '§'

fromFileEncoding :: FileEncoding -> CellContent
fromFileEncoding ' ' = Empty
fromFileEncoding '!' = LowSource
fromFileEncoding '"' = HighSource
fromFileEncoding '#' = UnknownOutput
fromFileEncoding '$' = HorizontalPath
fromFileEncoding '%' = VerticalPath
fromFileEncoding '&' = PathCross
fromFileEncoding '\'' = PathIntersection
fromFileEncoding '(' = VerticalRightPath
fromFileEncoding ')' = VerticalLeftPath
fromFileEncoding '*' = HorizontalUpPath
fromFileEncoding '+' = HorizontalDownPath
fromFileEncoding ',' = QuadrantThreePath
fromFileEncoding '-' = QuadrantTwoPath
fromFileEncoding '.' = QuadrantFourPath
fromFileEncoding '/' = QuadrantOnePath
fromFileEncoding '0' = HorizontalBufferLR
fromFileEncoding '1' = HorizontalBufferRL
fromFileEncoding '2' = HorizontalInverterLR
fromFileEncoding '3' = HorizontalInverterRL
fromFileEncoding '4' = HorizontalORInputLR
fromFileEncoding '5' = HorizontalORInputRL
fromFileEncoding '6' = HorizontalOROutputLR
fromFileEncoding '7' = HorizontalOROutputRL
fromFileEncoding '8' = HorizontalOR
fromFileEncoding '9' = HorizontalANDInputLR
fromFileEncoding ':' = HorizontalANDInputRL
fromFileEncoding ';' = HorizontalANDOutputLR
fromFileEncoding '<' = HorizontalANDOutputRL
fromFileEncoding '=' = HorizontalAND
fromFileEncoding '>' = HorizontalXORInputLR
fromFileEncoding '?' = HorizontalXORInputRL
fromFileEncoding '@' = HorizontalXOROutputLR
fromFileEncoding 'A' = HorizontalXOROutputRL
fromFileEncoding 'B' = HorizontalXOR
fromFileEncoding 'C' = HorizontalNORInputLR
fromFileEncoding 'D' = HorizontalNORInputRL
fromFileEncoding 'E' = HorizontalNOROutputLR
fromFileEncoding 'F' = HorizontalNOROutputRL
fromFileEncoding 'G' = HorizontalNOR
fromFileEncoding 'H' = HorizontalNANDInputLR
fromFileEncoding 'I' = HorizontalNANDInputRL
fromFileEncoding 'J' = HorizontalNANDOutputLR
fromFileEncoding 'K' = HorizontalNANDOutputRL
fromFileEncoding 'L' = HorizontalNAND
fromFileEncoding 'M' = HorizontalXNORInputLR
fromFileEncoding 'N' = HorizontalXNORInputRL
fromFileEncoding 'O' = HorizontalXNOROutputLR
fromFileEncoding 'P' = HorizontalXNOROutputRL
fromFileEncoding 'Q' = HorizontalXNOR
fromFileEncoding '§' = Empty