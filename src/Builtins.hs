module Builtins
  ( Builtin(..)
  , builtins
  )
where

import qualified Data.Map.Strict               as Map
import qualified Text.Show

import qualified Syntax.Atom                   as Syntax
import qualified Syntax.Reference              as Syntax
import qualified Type.Expression               as Type
import           Type.Types                     ( Type(..) )

data Builtin = Builtin
  { name     :: Text
  , function :: [Syntax.Atom] -> Syntax.Atom
  , typ      :: Type
  }

instance Show Builtin where
  show = show . name

instance Eq Builtin where
  (==) = (==) `on` name

instance Ord Builtin where
  compare = compare `on` name

getInteger :: Syntax.Atom -> Maybe Integer
getInteger (Syntax.Integer i) = Just i
getInteger _                  = Nothing

integerBinOp :: (Traversable t) => (t Integer -> Integer) -> t Syntax.Atom -> Syntax.Atom
integerBinOp op =
  fromMaybe (error "[builtins] couldn't evaluate builtin primitive")
    . map (Syntax.Integer . op)
    . traverse getInteger

builtins :: Map Syntax.Value Builtin
builtins = Map.fromList $ map ((Syntax.Local . name) &&& identity) [plus, minus, times]
 where
  plus = Builtin
    { name     = "+"
    , function = integerBinOp sum
    , typ      = Type.fn [Type.integer, Type.integer, Type.integer]
    }

  minus = Builtin
    { name     = "-"
    , function = integerBinOp $ \case
      []       -> 0
      [n     ] -> -n
      (n : ns) -> n - sum ns
    , typ      = Type.fn [Type.integer, Type.integer, Type.integer]
    }

  times = Builtin
    { name     = "*"
    , function = integerBinOp product
    , typ      = Type.fn [Type.integer, Type.integer, Type.integer]
    }

  -- println = Builtin
  --   { name = "println"
  --   , function = putTextLn . unwords . map show
  --   , typ = let alpha = Var "alpha" in Forall alpha Type (Type.fn [UniversalVariable alpha, Type.unit]) -- TODO: skolemization
  --   }
