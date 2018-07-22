module Error where

import           Text.Parsec (ParseError)

data Error
  = Parsing ParseError
  | Interpreting String
  deriving (Show, Eq)
