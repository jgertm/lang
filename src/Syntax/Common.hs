module Syntax.Common where


type Name = Text

newtype Binding =
  Single Name
  deriving (Show, Eq, Ord)
