{-# LANGUAGE UndecidableInstances #-}

module Syntax.Type where

import           Classes
import           Syntax.Common                  ( Name )

data Type phase
  = Product (Context phase)
            [Type phase]
  | Sum (Context phase)
        [Type phase]
  | Record (Context phase)
           [(Name, Type phase)]
  | Tag (Context phase)
        Name -- ^ keyword
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
