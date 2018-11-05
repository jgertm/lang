module Interpreter
  ( interpret
  , interpretWith
  , Env
  )
where

import           Control.Monad.Except
import qualified Data.Map.Strict               as M

import           Builtins
import           Error
import           Syntax

data Evaluation

type instance Context Evaluation = ()

type Term = Term' Evaluation

type Definition = Definition' Evaluation

type Type = Type' Evaluation

type Pattern = Pattern' Evaluation

type MonadInterpret m = (MonadReader Env m, MonadError InterpretationError m)

type Env = Map Binding Term

interpret :: Term' phase -> Either Error Term
interpret = interpretWith $ map (\bi -> ENative () bi []) builtins

interpretWith :: Env -> Term' phase -> Either Error Term
interpretWith env = runInterpreter env . eval . meta (const ())

runInterpreter :: Env -> _ a -> Either Error a
runInterpreter env = runExcept . withExcept Interpretation . usingReaderT env

replaceWith :: (Term -> Maybe Term) -> Term -> Term
replaceWith fn = descend (\vex -> fromMaybe vex $ fn vex)

replaceSymbol :: Binding -> Term -> Term -> Term
replaceSymbol binding substitution = replaceWith
  (\case
    ESymbol _ binding' ->
      if binding == binding' then Just substitution else Nothing
    _ -> Nothing
  )

replaceSymbols :: Map Binding Term -> Term -> Term
replaceSymbols dict = replaceWith
  (\case
    ESymbol _ binding -> M.lookup binding dict
    _                 -> Nothing
  )

eval :: (MonadInterpret m) => Term -> m Term
eval (EApplication _ fn args) = do
  args' <- traverse eval args
  fn'   <- eval fn
  eval =<< case fn' of
    ELambda _ argv lambdabody ->
      let dict        = M.fromList $ zip argv args'
          lambdabody' = replaceSymbols dict lambdabody
          argv'       = drop (length args) argv
      in  if null argv'
            then pure lambdabody'
            else pure $ ELambda def argv' lambdabody'
    ENative ctx builtin oldargs ->
      let atoms = map (\(EAtom _ atom) -> atom) args'
      in  eval $ ENative ctx builtin (oldargs <> atoms) -- FIXME: don't use `undefined` here
eval (ENative _ builtin args) = pure $ EAtom def $ function builtin args
eval (ELet _ bindings body) =
  let go ((name, value) : moreBindings) = do
        value' <- eval value
        local (M.insert name value') $ go moreBindings
      go [] = eval body
  in  go bindings
eval (EIf _ test true false) = do
  EAtom _ (ABoolean result) <- eval test
  eval $ if result then true else false
eval (ESymbol _ binding@(Single name)) = do
  env <- ask
  liftMaybe (UnboundSymbol name) $ M.lookup binding env
eval (EMatch _ prototype clauses) = do
  prototype'       <- eval prototype
  (bindings, body) <-
    liftMaybe NoMatchingPattern
    . getFirst
    . foldMap (\(pat, body) -> First . map (, body) $ match pat prototype')
    $ clauses
  local (`M.union` bindings) $ eval body
eval e = metaM (const pass) e

match :: Pattern' phase -> Term -> Maybe (Map Binding Term)
match (PWildcard _      ) _              = Just M.empty
match (PSymbol _ binding) expr           = Just $ M.singleton binding expr
match (PVector _ pv     ) (EVector _ ev) = do
  guard $ length pv == length ev
  map M.unions . sequenceA $ zipWith match pv ev
match (PAtom _ pat) (EAtom _ atm) =
  if pat == atm then Just M.empty else Nothing
match _ _ = Nothing
