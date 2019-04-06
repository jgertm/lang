module Error
  ( Error(..)
  , RenamingError(..)
  , InterpretationError(..)
  , ModuleError(..)
  , TypeError(..)
  )
where

import qualified Text.Parsec                   as Parsec

import qualified Syntax.Reference              as Syntax
import           Type.Types

data Error
  = Parsing Parsec.ParseError
  | Renaming RenamingError
  | Interpretation InterpretationError
  | Module ModuleError
  | Type TypeError
  deriving (Show, Eq)

data RenamingError =
  UnknownSymbol Syntax.Value
  deriving (Show, Eq)

data InterpretationError
  = Unimplemented
  | Semantics
  | UnboundSymbol Syntax.Value
  | NoMatchingPattern
  | Unknown
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
  | UnificationError
  | TypeMismatch Type Type
  | UnknownBinding Syntax.Value
  | KindMismatch Kind Kind
  | UnsolvedExistential (Variable 'Existential)
  | UnsolvedUniversal (Variable 'Universal)
  | InsufficientCoverage
  | PrincipalityRecoveryFailure (Set (Variable 'Existential))
  | RuleError String
  deriving (Show, Eq)
