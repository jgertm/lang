module Type.Match (check, covers) where

import Type.Monad (Infer)
import Type.Types


check :: Context -> [Branch] -> ([Type], Principality) -> (Type, Principality) -> Infer Context

covers :: Context -> [Branch] -> ([Type], Principality) -> Bool
