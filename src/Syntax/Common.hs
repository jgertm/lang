module Syntax.Common where

import qualified Data.Text                     as Text
import           Data.Text.Prettyprint.Doc


type Name = Text

data Binding
  = Single Name
  | Qualified [Name] Name
  deriving (Show, Eq, Ord)

instance Pretty Binding where
  pretty (Single name) = pretty name
  pretty (Qualified path name) = pretty $ mconcat [Text.intercalate "." path, "/", name]

nest :: Name -> Binding -> Binding
nest outer (Single name          ) = Qualified [outer] name
nest outer (Qualified outers name) = Qualified (outer : outers) name
