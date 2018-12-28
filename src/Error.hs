module Error
  ( Error(..)
  , RenamingError(..)
  , InterpretationError(..)
  , ModuleError(..)
  , TypeError(..)
  )
where

import qualified Text.Parsec                   as Parsec

import           Syntax.Common
import           Type.Types

data Error
  = Parsing Parsec.ParseError
  | Renaming RenamingError
  | Interpretation InterpretationError
  | Module ModuleError
  | Type TypeError
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

data ModuleError =
  NoMainFunction
  deriving (Show, Eq)

data TypeError
  = AnalysisError
  | ContextError
  | UnknownError String
  | InstantiationError
  | SubtypingError
  | SynthesisError
  | UnificationError
  | MatchError
  | TypeMismatch Type Type
  deriving (Show, Eq)
