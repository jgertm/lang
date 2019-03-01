module Test.Utils where

import qualified Data.Map.Strict               as Map

import           Classes
import qualified Parser
import qualified Renaming
import qualified Syntax.Atom                   as Atom
import qualified Syntax.Reference              as Reference
import qualified Syntax.Term                   as Term


data Test
type instance Context Test = ()
type instance Extra Test = ()
type Term = Term.Term Test

noContext :: Context Test
noContext = ()

unit :: Term
unit = Term.Atom noContext Atom.Unit

int :: Integer -> Term
int = Term.Atom noContext . Atom.Integer

var :: Text -> Reference.Value
var = Reference.Local

varIn :: [Text] -> Text -> Reference.Value
varIn path name = Reference.Remote (Reference.Module path) name

sym :: Text -> Term
sym = Term.Symbol noContext . Reference.Local

kw :: Text -> Reference.Keyword
kw = Reference.Keyword . var

kwIn :: [Text] -> Text -> Reference.Keyword
kwIn path name = Reference.Keyword $ varIn path name

lambda :: Text -> Term -> Term
lambda varname body = Term.Lambda noContext (var varname) body

parse = fromRight undefined . Parser.parse Parser.term "<test>"

rename = fromRight undefined . Renaming.rename

removeContext :: (Tree t p, Tree t Test) => t p -> t Test
removeContext = meta (const ())


tuple = Term.Tuple noContext . Map.fromList
record = Term.Record noContext . Map.fromList
variant tag body = Term.Variant noContext tag body
vector = Term.Vector noContext
