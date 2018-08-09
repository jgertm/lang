module Builtins where

import qualified Data.Map.Strict  as M
import qualified Universum.Unsafe as Unsafe

import           Syntax
import           Types

getInt :: Atom -> Maybe Int
getInt (AInteger i) = Just i
getInt _            = Nothing

plus =
  Builtin
    { name = "+"
    , function = Unsafe.fromJust . map (AInteger . sum) . traverse getInt
    , typ = fn [integer, integer, integer]
    }

times =
  Builtin
    { name = "*"
    , function = Unsafe.fromJust . map (AInteger . product) . traverse getInt
    , typ = fn [integer, integer, integer]
    }

builtins = M.fromList $ map ((Single . name) &&& identity) [plus, times]
