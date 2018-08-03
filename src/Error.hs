module Error
  ( Error(..)
  , RenamingError(..)
  , InterpretationError(..)
  , InferenceError(..)
  ) where

import qualified Text.Parsec as Parsec

import           Syntax
import           Types

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
  deriving (Show, Eq)

data InferenceError =
  TypeMismatch Type
               Type
  deriving (Show, Eq)
