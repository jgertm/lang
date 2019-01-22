module Test.Utils where

import           Classes
import qualified Parser
import qualified Renaming
import qualified Syntax.Atom                   as Atom
import qualified Syntax.Common                 as Common
import qualified Syntax.Term                   as Term

data Test
type instance Context Test = ()
type instance Extra Test = ()
type Term = Term.Term Test

noContext :: Context Test
noContext = ()

unit :: Term
unit = Term.Atom noContext Atom.Unit

int :: Int -> Term
int = Term.Atom noContext . Atom.Integer

var :: Common.Name -> Common.Binding
var = Common.Single

sym :: Common.Name -> Term
sym = Term.Symbol noContext . Common.Single

lambda :: Common.Name -> Term -> Term
lambda varname body = Term.Lambda noContext (var varname) body

parse = fromRight undefined . Parser.parse Parser.term "<test>"

rename = fromRight undefined . Renaming.rename

removeContext :: (Tree t p, Tree t Test) => t p -> t Test
removeContext = meta (const ())
