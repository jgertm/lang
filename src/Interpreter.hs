module Interpreter
  ( interpret
  , interpretWith
  , Env
  )
where

import qualified Data.Map.Strict               as M

import           Builtins
import           Classes
import           Error
import qualified Syntax.Atom                   as Atom
import qualified Syntax.Common                 as Common
import qualified Syntax.Definition             as Definition
import qualified Syntax.Pattern                as Pattern
import qualified Syntax.Term                   as Term
import qualified Syntax.Type                   as Type


data Evaluation

type instance Context Evaluation = ()

type Term = Term.Term Evaluation
type Definition = Definition.Definition Evaluation
type Type = Type.Type Evaluation
type Pattern = Pattern.Pattern Evaluation

type MonadInterpret m = (MonadReader Env m, MonadError InterpretationError m)

type Env = Map Common.Binding Term

interpret :: Term.Term phase -> Either Error Term
-- interpret = interpretWith $ map (\bi -> Term.Native () bi []) builtins
interpret = interpretWith mempty

interpretWith :: Env -> Term.Term phase -> Either Error Term
interpretWith env = runInterpreter env . eval . meta (const ())

runInterpreter :: Env -> _ a -> Either Error a
runInterpreter env = runExcept . withExcept Interpretation . usingReaderT env

replaceWith :: (Term -> Maybe Term) -> Term -> Term
replaceWith fn = descend (\vex -> fromMaybe vex $ fn vex)

replaceSymbol :: Common.Binding -> Term -> Term -> Term
replaceSymbol binding substitution = replaceWith
  (\case
    Term.Symbol _ binding' -> if binding == binding' then Just substitution else Nothing
    _                      -> Nothing
  )

replaceSymbols :: Map Common.Binding Term -> Term -> Term
replaceSymbols dict = replaceWith
  (\case
    Term.Symbol _ binding -> M.lookup binding dict
    _                     -> Nothing
  )

eval :: (MonadInterpret m) => Term -> m Term
-- eval (Term.Application _ fn args) = do
--   args' <- traverse eval args
--   fn'   <- eval fn
--   eval =<< case fn' of
--     Term.Lambda _ argv lambdabody ->
--       let dict        = M.fromList $ zip argv args'
--           lambdabody' = replaceSymbols dict lambdabody
--           argv'       = drop (length args) argv
--       in  if null argv' then pure lambdabody' else pure $ Term.Lambda () argv' lambdabody'
--     Term.Native ctx builtin oldargs ->
--       let atoms = map (\(Term.Atom _ atom) -> atom) args'
--       in  eval $ Term.Native ctx builtin (oldargs <> atoms)
-- eval (Term.Native _ builtin args) = pure $ Term.Atom () $ function builtin args
eval (Term.Let _ bindings body) =
  let go ((name, value) : moreBindings) = do
        value' <- eval value
        local (M.insert name value') $ go moreBindings
      go [] = eval body
  in  go bindings
eval (Term.If _ test true false) = do
  result <- eval test
  case result of
    Term.Atom _ (Atom.Boolean result) -> eval $ if result then true else false
    other                             -> error "Condition invoked with something other than boolean"
eval (Term.Symbol _ binding@(Common.Single name)) = do
  env <- ask
  liftMaybe (UnboundSymbol name) $ M.lookup binding env
-- eval (Term.Match _ prototype branches) = do
--   prototype'       <- eval prototype
--   (bindings, body) <-
--     liftMaybe NoMatchingPattern
--     . getFirst
--     . foldMap (\(pat, body) -> First . map (, body) $ match pat prototype')
--     $ branches
--   local (`M.union` bindings) $ eval body
eval e = metaM (const pass) e

match :: Pattern.Pattern phase -> Term -> Maybe (Map Common.Binding Term)
match (Pattern.Wildcard _      ) _                  = Just M.empty
match (Pattern.Symbol _ binding) expr               = Just $ M.singleton binding expr
match (Pattern.Vector _ pv     ) (Term.Vector _ ev) = do
  guard $ length pv == length ev
  map M.unions . sequenceA $ zipWith match pv ev
match (Pattern.Atom _ pat) (Term.Atom _ atm) = if pat == atm then Just M.empty else Nothing
match _                    _                 = Nothing
