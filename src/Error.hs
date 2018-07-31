module Error
  ( Error(..)
  , InterpretationError(..)
  , InferenceError(..)
  ) where

import qualified Text.Parsec as Parsec
import           Types

data Error
  = Parsing Parsec.ParseError
  | Interpretation InterpretationError
  | Inference InferenceError
  deriving (Show, Eq)

data InterpretationError
  = Unimplemented
  | Semantics
  deriving (Show, Eq)

data InferenceError =
  TypeMismatch Type
               Type
  deriving (Show, Eq)
