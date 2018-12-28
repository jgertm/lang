module Syntax.Atom where


data Atom
  = Unit
  | Integer Int
  | String Text
  | Boolean Bool
  deriving (Show, Eq, Ord, Generic)

render :: (IsString s) => Atom -> s
render Unit        = "nil"
render (Integer i) = show i
render (String  s) = show s
render (Boolean b) = if b then "true" else "false"
