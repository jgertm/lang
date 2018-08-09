module Types where

import           Data.Map  (Map)
import           Data.Text (Text)

newtype Metavar =
  MV Int
  deriving (Show, Eq, Ord, Enum)

type Name = Text

data Type
  = Primitive PrimitiveType
  | Application Type
                Type
  | Constructor Name
  | Higher Type
           Type
  | Function Type
             Type
  | Product [Type]
  | Record (Map Name Type)
  | Variant (Map Name Type)
  | Metavariable Metavar
  | Bottom
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
vector = Higher (Constructor "vector")

fn :: [Type] -> Type
fn = foldr1 Function
