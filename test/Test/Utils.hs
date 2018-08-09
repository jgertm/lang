module Test.Utils where

import qualified Parser
import qualified Renaming
import           Syntax

data Test

type instance Context Test = ()

noContext = ()

unit :: Term' Test
unit = EAtom noContext AUnit

int :: Int -> Term' Test
int = EAtom noContext . AInteger

var = Single

sym = ESymbol noContext . Single

parse = fromRight undefined . Parser.parse Parser.term "<test>"

rename = fromRight undefined . Renaming.rename

removeContext :: (Tree t p, Tree t Test) => t p -> t Test
removeContext = meta (const ())
