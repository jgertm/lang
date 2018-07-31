module Builtins where

import           Data.Map.Lazy

import           Syntax.Abstract

getInt :: Atom -> Maybe Int
getInt (AInteger i) = Just i
getInt _            = Nothing

functions :: Map Name ([Atom] -> Maybe Atom)
functions =
  [ ("+", fmap (AInteger . sum) . traverse getInt)
  , ("*", fmap (AInteger . product) . traverse getInt)
  ]
