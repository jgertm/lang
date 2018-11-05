{-# LANGUAGE UndecidableInstances #-}

module Inference
  ( infer
  , inferWith
  , inferDefinitionWith
  , check
  , checkWith
  , Definition
  )
where

import           Data.Generics.Product.Typed
import           Data.Map.Merge.Strict
import qualified Data.Map.Strict               as Map
import           Data.Partition                 ( Partition )
import qualified Data.Partition                as Disj
import qualified Data.Set                      as Set

import qualified Builtins
import           Error
import           Syntax
import           Types

data Inferring

type instance Context Inferring = Metavar

data Inferred

type instance Context Inferred = Type

type Definition = Definition' Inferred

infer = inferWith (map typ Builtins.builtins)

inferWith :: Bindings -> Term' phase -> Either Error (Term' Inferred, State)
inferWith env node =
  runExcept
    . withExcept Inference
    . usingStateT
        (State
          { nextMetavariable = MV 0
          , solution         = mempty
          , equivalences     = Disj.empty
          }
        )
    . (applySolution <=< solve <=< assignMetavars)
    $ node
 where
  solve node' =
    usingReaderT (Scope {current = node', bindings = env}) $ solveAST node'

inferDefinitionWith env (DConstant _ name body) = do
  (body', _) <- inferWith env body
  let typ = context body'
  pure $ DConstant typ name body'

check = map fst . infer

checkWith = map fst ... inferWith

type MonadInferenceState m = MonadState State m

type MonadInferenceError m = MonadError InferenceError m

type MonadScope t m = MonadReader (Scope t) m

type MonadInfer m = (MonadInferenceState m, MonadInferenceError m)

data State = State
  { nextMetavariable :: Metavar
  , solution         :: Solution
  , equivalences     :: Partition Metavar
  } deriving (Show, Eq, Generic)

data Scope t = Scope
  { current  :: t Inferring
  , bindings :: Bindings
  } deriving (Generic)

deriving instance (Show (t Inferring)) => Show (Scope t)

deriving instance (Eq (t Inferring)) => Eq (Scope t)

metavar :: (Tree t Inferring) => t Inferring -> Metavar
metavar = context

metatype :: (Tree t Inferring) => t Inferring -> Type
metatype = Metavariable . context

type Bindings = Map Binding Type

lookupBinding binding = do
  env <- view (typed @Bindings)
  maybe (throwError $ UnknownBinding binding) pure $ Map.lookup binding env

type Solution = Map Metavar Type

lookupSolution mv = do
  env <- use (typed @Solution)
  mvs <- siblings mv
  let result = asum $ map (flip Map.lookup env) (mv : mvs)
  pure result

insertSolution mv typ = modifying (typed @Solution) $ Map.insert mv typ

combineMetavars mv1 mv2 =
  modifying (typed @(Partition Metavar)) $ Disj.joinElems mv1 mv2

freshMetavar = do
  mv <- use (typed @Metavar)
  modifying (typed @Metavar) succ
  pure mv

freshMetatype = Metavariable <$> freshMetavar

assignMetavars = Syntax.metaM $ const freshMetavar

metavarHere = context <$> asks current

typeHere = Metavariable <$> metavarHere

solveAST node = do
  solveTerm node
  pure node

solveTerm (ESymbol _ binding       ) = updateHere =<< lookupBinding binding
solveTerm (ELambda _ arguments body) = do
  argumentTypes <- traverse (const freshMetatype) arguments
  let returnType  = metatype body
      newBindings = Map.fromList $ zip arguments argumentTypes
  updateHere $ Types.fn $ argumentTypes <> [returnType]
  local (set #current body . over #bindings (`mappend` newBindings))
    $ solveTerm body
solveTerm (EApplication _ function arguments) = do
  returnType <- typeHere
  updateHere returnType
  let argumentTypes = map metatype arguments
      functionType  = Types.fn $ argumentTypes <> [returnType]
      functionMV    = metavar function
  update functionMV functionType
  local (set #current function) $ solveTerm function
  traverse_ (\arg -> local (set #current arg) $ solveTerm arg) arguments
solveTerm (EIf _ test thn els) = do
  let testMV = metavar test
  update testMV (Primitive Boolean)
  traverse_ (updateHere . metatype) [thn, els]
  traverse_ (\term -> local (set #current term) $ solveTerm term)
            [test, thn, els]
solveTerm (EMatch _ proto clauses) = do
  let protoMV = metavar proto
  local (set #current proto) $ solveTerm proto
  forM_
    clauses
    (\(pat, body) -> do
      (typ, newBindings) <- patternType pat
      update (metavar pat) typ
      update protoMV       typ
      updateHere (metatype body)
      local (set #current body . over #bindings (`mappend` newBindings))
        $ solveTerm body
    )
solveTerm (EAtom _ atom        ) = updateHere (Primitive $ primitiveType atom)
solveTerm (ELet _ bindings body) = do
  updateHere $ metatype body
  let newBindings = Map.fromList $ map (fst &&& (metatype . snd)) bindings
  traverse_ (\(_, term) -> local (set #current term) $ solveTerm term) bindings
  local (set #current body . over #bindings (`mappend` newBindings))
    $ solveTerm body

unify' _ t@(Primitive pt1) (Primitive pt2) | pt1 == pt2 = pure t
unify' _ (Application con1 args1) (Application con2 args2)
  | con1 == con2 && length args1 == length args2 = do
    args' <- zipWithM (unify' True) args1 args2
    pure $ Application con2 args'
unify' _ (Metavariable mv1) (Metavariable mv2) = do
  combineMetavars mv1 mv2
  fromMaybe (Metavariable (min mv1 mv2)) <$> lookupSolution mv2
-- unify' _ (Tuple ts1) (Tuple ts2)
--   | length ts1 == length ts2 = Tuple <$> zipWithM (unify' True) ts1 ts2
-- TODO: i don't think this is right, records and variants need to implement a subtyping check
-- unify' _ (Record rs1) (Record rs2) =
--   let combineRows _ r1 r2 =
--         if r1 == r2
--           then pure r1
--           else pass -- throwError $ UnificationFailure r1 r2
--    in Record <$>
--       mergeA
--         preserveMissing
--         preserveMissing
--         (zipWithAMatched combineRows)
--         rs1
--         rs2
unify' prop (Metavariable mv) t2 = do
  current <- lookupSolution mv
  new     <- case current of
    Nothing -> pure t2
    Just tc -> unify' True t2 tc
  insertSolution mv new
  when prop $ traverse_ (unify' False t2 . Metavariable) =<< siblings mv
  pure new
unify' prop t  mv@(Metavariable _) = unify' prop mv t
unify' _    t1 t2                  = throwError $ UnificationFailure t1 t2

unify = unify' True

update mv typ = void $ unify (Metavariable mv) typ

updateHere typ = do
  mv <- metavarHere
  update mv typ

siblings mv = do
  State { equivalences } <- get
  pure $ Set.toList $ Set.delete mv $ Disj.find equivalences mv

applySolution =
  Syntax.metaM $ \mv -> liftMaybe (UnknownVariable mv) =<< lookupSolution mv

primitiveType = \case
  AUnit      -> Unit
  AInteger _ -> Integer
  AString  _ -> String
  ABoolean _ -> Boolean

patternType (    PAtom _ atom) = pure (Primitive $ primitiveType atom, mempty)
patternType pat@(PWildcard _ ) = pure (metatype pat, mempty)
patternType pat@(PSymbol _ binding) =
  let typ = metatype pat in pure (typ, Map.singleton binding typ)
patternType pat@(PVector _ ps) = foldM
  (\(t, env) p -> do
    (t', env') <- patternType p
    (,) <$> unify t t' <*> (pure $ env <> env')
  )
  (metatype pat, mempty)
  ps
