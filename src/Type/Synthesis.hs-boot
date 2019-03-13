module Type.Synthesis (synthesize, atom) where

import Type.Monad
import Type.Types

synthesize :: Context -> Term -> Infer ((Type, Principality), Context)

atom :: Atom -> Type
