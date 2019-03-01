module Syntax.Atom where

import           Data.Text.Prettyprint.Doc


data Atom
  = Unit
  | Integer Integer
  | String Text
  | Boolean Bool
  deriving (Show, Eq, Ord, Generic)

instance Pretty Atom where
  pretty Unit        = "nil"
  pretty (Integer i) = pretty $ show i
  pretty (String  s) = pretty $ show s
  pretty (Boolean b) = if b then "true" else "false"
