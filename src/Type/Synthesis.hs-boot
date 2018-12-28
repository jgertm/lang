module Type.Synthesis where

import Type.Monad
import Type.Types

synthesize :: Context -> Term -> Infer ((Type, Principality), Context)
