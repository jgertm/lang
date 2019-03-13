module Interpreter.Types
  ( Evaluation
  , Pattern
  , Term
  , C
  , E
  , K
  , CEK
  , Native(..)
  , Closure(..)
  , Continuation(..)
  )
where

import qualified Builtins
import           Classes
import qualified Syntax.Atom                   as Atom
import qualified Syntax.Pattern                as Pattern
import qualified Syntax.Reference              as Reference
import qualified Syntax.Term                   as Term


data Evaluation

type instance Context Evaluation = ()
type instance Extra Evaluation = Native

type Term = Term.Term Evaluation
type Pattern = Pattern.Pattern Evaluation
type Branch = Term.Branch Evaluation

data Native = Native Builtins.Builtin [Atom.Atom] deriving (Show, Eq, Ord)

data Closure = Closure C E deriving (Show, Eq)

data Continuation
  = Done
  | Arguments [C] E K
  | Call C E K
  | Conditional C C K
  | Bind Reference.Value C K
  | Select [Branch] E K
  | Error K
  deriving (Show)

type C = Term
type E = Map Reference.Value Closure
type K = Continuation
type CEK = (C,E,K)
