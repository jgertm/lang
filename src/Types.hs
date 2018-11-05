module Types where

import           Data.Map                       ( Map )
import           Data.Text                      ( Text )

newtype Metavar =
  MV Int
  deriving (Show, Eq, Ord, Enum)

type Name = Text

data Type
  = Primitive PrimitiveType
  | Application Name
                [Type]
  | Product [Type]
  | Record (Map Name Type)
  | Variant (Map Name Type)
  | Variable Name
  | Forall [Name]
           Type
  | Metavariable Metavar
  deriving (Show, Eq, Ord)

data PrimitiveType
  = Unit
  | Integer
  | String
  | Boolean
  deriving (Show, Eq, Ord)

unit, integer, string, boolean :: Type
unit = Primitive Unit

integer = Primitive Integer

string = Primitive String

boolean = Primitive Boolean

vector :: Type -> Type
vector = Application "vector" . one

fn :: [Type] -> Type
fn = Application "fn"
