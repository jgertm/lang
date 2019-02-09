{-# LANGUAGE UndecidableInstances #-}

module Syntax.Type where

import           Classes
import qualified Syntax.Reference              as Syntax

data Type phase
  = Named (Context phase) Syntax.Type
  | Product (Context phase)
            [Type phase]
  | Sum (Context phase)
        [Type phase]
  | Record (Context phase)
           [(Syntax.Keyword, Type phase)]
  | Variant (Context phase)
        Syntax.Keyword
        (Type phase)
  | Function (Context phase)
             [Type phase]
  deriving (Generic)

deriving instance (Show (Context phase)) => Show (Type phase)

deriving instance (Eq (Context phase)) => Eq (Type phase)

deriving instance (Ord (Context phase)) => Ord (Type phase)

instance Tree Type phase where
  walkM = undefined
  metaM = undefined
