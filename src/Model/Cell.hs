module Model.Cell where

data Cell
  = Empty
  | LowSource
  | HighSource
  | UnknownOutput
  | LowOutput
  | HighOutput --TODO: Add the remaining possible states
  deriving (Eq)

instance Show Cell where
  show c = toString c

toString :: Cell -> String
toString Empty = "   "
toString LowSource = "(0)"
toString HighSource = "(1)"
toString UnknownOutput = "(?)"
toString LowOutput = "(0)"
toString HighOutput = "(1)"