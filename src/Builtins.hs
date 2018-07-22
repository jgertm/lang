module Builtins where

import           Data.Map.Lazy
import           Data.Text       (Text)

import           Syntax.Abstract

getInt :: Atom -> Maybe Int
getInt (AInteger i) = Just i
getInt _            = Nothing

functions :: Map Name ([Atom] -> Maybe Atom)
functions =
  [ ("+", (\args -> AInteger . sum <$> traverse getInt args))
  , ("*", (\args -> AInteger . product <$> traverse getInt args))
  ]
