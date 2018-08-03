module Annotation where

import           Text.Parsec (SourcePos)

import           Types       (Type)

data Meta
  = Extent SourcePos
           SourcePos
  | Type Type
  deriving (Show, Eq, Ord)
