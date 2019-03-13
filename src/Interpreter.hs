module Interpreter
  ( eval
  , evalWith
  )
where

import qualified Data.Map.Strict               as Map

import qualified Builtins
import           Classes
import           Error
import           Interpreter.Match
import           Interpreter.Types
import qualified Syntax.Atom                   as Atom
import qualified Syntax.Reference              as Reference
import qualified Syntax.Term                   as Term


lookup :: Reference.Value -> E -> Maybe Closure
lookup sym env = Map.lookup sym env

insert :: Reference.Value -> Closure -> E -> E
insert name closure env = Map.insert name closure env


step :: C -> E -> K -> CEK

step term@(Term.Symbol _ symbol) env k = case lookup symbol env of
  Nothing               -> (term, env, Error k)
  Just (Closure t env') -> (t, env', k)
step term@(Term.Fix _ recur body) env k =
  let env' = insert recur (Closure term env) env in (body, env', k)
step (Term.If _ test true false) env k = (test, env, Conditional true false k)
step (Term.Atom _ (Atom.Boolean boolean)) env (Conditional true false k) =
  let next = if boolean then true else false in (next, env, k)

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
step (Term.Atom () result) env (Call (Term.Extra () (Native builtin args)) _ k) = case k of
  Arguments{} -> (Term.Extra () $ Native builtin (args <> [result]), env, k)
  _           -> (Term.Atom () (Builtins.function builtin $ args <> [result]), env, k)

step (Term.Match _ prototype branches) env k = (prototype, env, Select branches env k)
step prototype _ (Select (Term.Branch { patterns, body } : branches) env k) =
  case asum $ map (`match` prototype) patterns of
    Nothing       -> (prototype, env, Select branches env k)
    Just bindings -> (body, env <> map (`Closure` mempty) bindings, k)

step c e Done = (c, e, Done)

step c e k    = (c, e, Error k)


isFinal :: CEK -> Bool
isFinal (_   , _, Error _) = True
isFinal (term, _, Done   ) = case term of
  Term.Application{} -> False
  Term.Symbol{}      -> False
  Term.If{}          -> False
  Term.Let{}         -> False
  Term.Match{}       -> False
  Term.Fix{}         -> False
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

evalTrace :: Term.Term phase -> [(Int, CEK)]
evalTrace term = case break isFinal $ run $ meta (const ()) term of
  (steps, final : _) -> zip [0 ..] $ steps <> [final]
  _                  -> error "[interpreter] invalid interpretation trace"

builtins :: Map Reference.Value Closure
builtins = map (\builtin -> Closure (Term.Extra () $ Native builtin []) mempty) Builtins.builtins
