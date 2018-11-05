module Builtins where

import qualified Data.Map.Strict               as M
import qualified Universum.Unsafe              as Unsafe

import           Syntax
import           Types

getInt :: Atom -> Maybe Int
getInt (AInteger i) = Just i
getInt _            = Nothing

integerBinOp op = Unsafe.fromJust . map (AInteger . op) . traverse getInt

plus = Builtin
  { name     = "+"
  , function = integerBinOp sum
  , typ      = fn [integer, integer, integer]
  }

minus = Builtin
  { name     = "-"
  , function = integerBinOp $ \(n : ns) -> n - sum ns
  , typ      = fn [integer, integer, integer]
  }

times = Builtin
  { name     = "*"
  , function = integerBinOp product
  , typ      = fn [integer, integer, integer]
  }

builtins = M.fromList $ map ((Single . name) &&& identity) [plus, minus, times]
