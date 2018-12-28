module Type.Types where

import qualified Classes
import qualified Syntax.Common                 as Syntax
import qualified Syntax.Pattern                as Syntax
import qualified Syntax.Term                   as Syntax


type Term = Syntax.Term Classes.Empty
type Pattern = Syntax.Pattern Classes.Empty
type Branch = Syntax.Branch Classes.Empty

newtype Context = Context
  { unContext :: Seq Fact
  } deriving (Show, Eq, Ord)

data Fact
  = DeclareUniversal (Variable 'Universal)
                     Kind
  | DeclareExistential (Variable 'Existential)
                       Kind
  | Binding Syntax.Binding
            Type
            Principality
  | SolvedUniversal (Variable 'Universal)
                    Type
  | SolvedExistential (Variable 'Existential)
                      Kind
                      Type
  | Marker (Variable 'Existential)
  deriving (Show, Eq, Ord, Generic)

data Principality
  = Principal
  | Nonprincipal
  deriving (Show, Eq, Ord, Generic)

data Quantification
  = Existential
  | Universal

newtype Variable (quantification :: Quantification) =
  Var Text
  deriving (Show, Eq, Ord, Generic)

data Kind
  = Type
  | Natural
  deriving (Show, Eq, Ord, Generic)

data Type
  = Primitive Text
  | Function Type Type
  | Variant (Map Syntax.Name Type)
  | Tuple (Map Int Type)
  | Record (Map Syntax.Name Type)
  | UniversalVariable (Variable 'Universal)
  | ExistentialVariable (Variable 'Existential)
  | Forall (Variable 'Universal)
           Kind
           Type
  | Exists (Variable 'Universal)
           Kind
           Type
  | Implies Proposition
            Type
  | With Type
         Proposition
  | Zero
  | Succ Type
  | Vector Type
           Type
  deriving (Show, Eq, Ord, Generic)

data Proposition =
  Equals Type
         Type
  deriving (Show, Eq, Ord, Generic)

data Polarity
  = Positive
  | Negative
  | Neutral
  deriving (Show, Eq)
