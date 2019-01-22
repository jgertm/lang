module Interpreter where

import qualified Data.Map.Strict               as Map
import           Data.Text.Prettyprint.Doc

import qualified Builtins
import           Classes
import           Error
import           Interpreter.Match
import           Interpreter.Types
import qualified Syntax.Atom                   as Atom
import qualified Syntax.Common                 as Common
import qualified Syntax.Term                   as Term


lookup :: Common.Binding -> E -> Closure
lookup sym env =
  fromMaybe (error $ "[interpreter] failed to lookup " <> show (squotes $ pretty sym))
    $ Map.lookup sym env

insert :: Common.Binding -> Closure -> E -> E
insert name closure env = Map.insert name closure env


step :: C -> E -> K -> CEK

step (Term.Symbol _ symbol     ) env k = let Closure t env' = lookup symbol env in (t, env', k)

step (Term.If _ test true false) env k = (test, env, Conditional true false k)
step (Term.Atom _ (Atom.Boolean bool)) env (Conditional true false k) =
  let next = if bool then true else false in (next, env, k)

step (Term.Let _ ((name, value) : bindings) body) env k =
  let k' = case bindings of
        []   -> Bind name body k
        more -> Bind name (Term.Let () more body) k
  in  (value, env, k')
step result env (Bind name c k) = (c, insert name (Closure result env) env, k)

step (Term.Application _ fn args) env k = (fn, env, Arguments args env k)
step fn env (Arguments args env' k) = case args of
  [arg       ] -> (arg, env', Call fn env k)
  (arg : rest) -> (arg, env', Call fn env (Arguments rest env' k))
step result env (Call (Term.Lambda _ arg body) env' k) =
  let env'' = insert arg (Closure result env) env' in (body, env'', k)
step (Term.Atom () result) env (Call (Term.Extra () (Native bi args)) _ k) = case k of
  Arguments{} -> (Term.Extra () $ Native bi (args <> [result]), env, k)
  _           -> (Term.Atom () (Builtins.function bi $ args <> [result]), env, k)

step (Term.Match _ prototype branches) env k = (prototype, env, Select branches k)
step prototype env (Select ((patterns, body) : branches) k) =
  case concatMapM (`match` prototype) patterns of
    Nothing       -> (prototype, env, Select branches k)
    Just bindings -> (body, env <> map (`Closure` env) bindings, k)

step c e Done = (c, e, Done)


isFinal :: CEK -> Bool
isFinal (term, _, Done) = case term of
  Term.Application{} -> False
  Term.Symbol{}      -> False
  Term.If{}          -> False
  Term.Let{}         -> False
  Term.Match{}       -> False
  _                  -> True
isFinal _ = False

run :: C -> [CEK]
run = runWith mempty

runWith :: E -> C -> [CEK]
runWith env term = iterate (\(c, e, k) -> step c e k) (term, Map.union env builtins, Done)

eval :: Term.Term phase -> Either Error Term
eval = evalWith mempty

evalWith :: E -> Term.Term phase -> Either Error Term
evalWith env term = case find isFinal $ runWith env $ meta (const ()) term of
  Just (result, _, _) -> Right result
  Nothing             -> Left $ Interpretation Unimplemented

evalTrace :: C -> [CEK]
evalTrace term = let (steps, final : _) = break isFinal $ run term in steps <> [final]

builtins :: Map Common.Binding Closure
builtins = map (\bi -> Closure (Term.Extra () $ Native bi []) mempty) Builtins.builtins
