module Builtins where

import qualified Data.Map.Strict               as M
import qualified Text.Show
import qualified Universum.Unsafe              as Unsafe

import qualified Syntax.Atom                   as Syntax
import qualified Syntax.Common                 as Syntax
import qualified Type.Expression               as Type
import           Type.Types                     ( Type )

data Builtin = Builtin
  { name     :: Syntax.Name
  , function :: [Syntax.Atom] -> Syntax.Atom
  , typ      :: Type
  }

instance Show Builtin where
  show = show . name

instance Eq Builtin where
  x == y = name x == name y

instance Ord Builtin where
  x `compare` y = name x `compare` name y

getInt :: Syntax.Atom -> Maybe Int
getInt (Syntax.Integer i) = Just i
getInt _                  = Nothing

integerBinOp :: (Traversable t) => (t Int -> Int) -> t Syntax.Atom -> Syntax.Atom
integerBinOp op = Unsafe.fromJust . map (Syntax.Integer . op) . traverse getInt

builtins :: Map Syntax.Binding Builtin
builtins = M.fromList $ map ((Syntax.Single . name) &&& identity) [plus, minus, times]
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
