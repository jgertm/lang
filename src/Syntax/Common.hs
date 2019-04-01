module Syntax.Common
  ( Extent(..)
  )
where


data Extent = Open | Closed deriving (Generic, Show, Eq, Ord)
