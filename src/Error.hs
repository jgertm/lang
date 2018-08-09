module Error
  ( Error(..)
  , RenamingError(..)
  , InterpretationError(..)
  , InferenceError(..)
  ) where

import qualified Text.Parsec as Parsec

import           Syntax
import           Types       (Metavar, Type)

data Error
  = Parsing Parsec.ParseError
  | Renaming RenamingError
  | Interpretation InterpretationError
  | Inference InferenceError
  deriving (Show, Eq)

data RenamingError =
  UnknownSymbol Name
  deriving (Show, Eq)

data InterpretationError
  = Unimplemented
  | Semantics
  | UnboundSymbol Name
  | NoMatchingPattern
  deriving (Show, Eq)

data InferenceError
  = UnificationFailure Type
                       Type
  | UnknownBinding Syntax.Binding
  | UnknownVariable Metavar
  deriving (Show, Eq)
