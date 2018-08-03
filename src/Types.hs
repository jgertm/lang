module Types where

import           Data.Map  (Map)
import           Data.Text (Text)

type TVar = Text

data Type
  = Variable TVar
  | Simple Text
  | Higher Type
           Type
  | Function Type
             Type
  deriving (Show, Eq, Ord)

data Kind
  = KStar
  | KList [Type]
  | KMap (Map Text Type)
  | KSet [Type]
  deriving (Show, Eq, Ord) -- TODO: higher kinds

unit, integer, string, boolean :: Type
unit = Simple "unit"

integer = Simple "integer"

string = Simple "string"

boolean = Simple "boolean"

vector :: Type -> Type
vector = Higher (Simple "vector")
--
-- data Type
--   = Atomic AtomicType
--   | Product [Type]
--   | Sum [(Identifier, Maybe Type)]
--   | Record [(Identifier, Type)]
--   deriving (Show, Eq)
-- data AtomicType
--   = UnitT
--   | IntegerT
--   | StringT
--   | KeywordT
--   deriving (Show, Eq)
