{-|
This module implements the type __CHECKING__ (@<=@) rules from /figure 14a:: Algorithmic typing, including rules omitted from main paper/ (page 37)
-}
module Type.Analysis where

import qualified Data.Map.Merge.Strict         as Map
import qualified Data.Map.Strict               as Map
import qualified Data.Sequence                 as Seq
import qualified Data.Set                      as Set

import           Error                          ( TypeError(..) )
import qualified Syntax.Atom                   as Syntax
import qualified Syntax.Term                   as Syntax
import qualified Type.Context                  as Ctx
import qualified Type.Equation                 as Equation
import           Type.Expression
import {-# SOURCE #-} qualified Type.Match     as Match
import           Type.Monad
import           Type.Rules
import           Type.Subtyping                 ( subtype )
import qualified Type.Subtyping                as Sub
import {-# SOURCE #-} Type.Synthesis            ( synthesize )
import           Type.Types

check, check' :: Context -> Term -> (Type, Principality) -> Infer Context

check gamma e ap = do
  judgement $ TypeChecking gamma e ap
  check' gamma e ap

check' gamma term (ExistentialVariable alpha, Nonprincipal)
  | alpha `Map.member` Ctx.existentialSolutions gamma
  = case Map.lookup alpha (Ctx.existentialSolutions gamma) of
    Just (solution, Type) -> check gamma term (solution, Nonprincipal)
    Just (_       , kind) -> throwError $ KindMismatch Type kind
    Nothing               -> throwError $ UnsolvedExistential alpha
-- RULE: Rec
check' gamma (Syntax.Fix _ x v) ap@(a, p) = do
  let binding = Binding x a p
  gamma' <- check (Ctx.add gamma binding) v ap
  pure $ Ctx.drop gamma' binding
-- RULE: 1Iα̂  (Atom introduction existential)
check' gamma (Syntax.Atom _ atom) (ExistentialVariable alpha, Nonprincipal)
  | (alpha, Type) `Set.member` Ctx.existentials gamma = do
    let typ         = atomType atom
        (pre, post) = Ctx.split gamma (DeclareExistential alpha Type)
    pure $ Ctx.inject pre (SolvedExistential alpha Type typ) post
-- RULE: 1I (Atom introduction)
check' gamma (Syntax.Atom _ atom) (typ, _) = do
  let typ' = atomType atom
  unless (typ == typ') $ throwError (TypeMismatch typ typ')
  pure gamma
-- RULE: Let [2013 paper]
check' gamma (Syntax.Let _ bindings body) cp = do
  gamma' <- foldM
    (\ctx (name, term) -> do
      ((a, p), ctx') <- synthesize ctx term
      let binding = Binding name a p
      pure $ Ctx.add ctx' binding
    )
    gamma
    bindings
  check gamma' body cp
-- RULE: If
check' gamma (Syntax.If _ test true false) ap = do
  theta <- check gamma test (boolean, Principal)
  alpha <- freshExistential
  let theta' = Ctx.add theta (DeclareExistential alpha Type)
  foldM (\ctx branch -> check ctx branch ap) theta' [true, false]
-- RULE: ∀I (Forall introduction)
check' gamma v (Forall alpha k a, p) = do
  unless (checkedIntroduction v) $ throwError AnalysisError
  let declaration = DeclareUniversal alpha k
  gamma' <- check (Ctx.add gamma declaration) v (a, p)
  pure $ Ctx.drop gamma' declaration
-- RULE: ∃I (Exists introduction)
check' gamma e (Exists alpha k a, _) = do
  unless (checkedIntroduction e) $ throwError AnalysisError
  alphaEx <- freshExistential
  delta   <- check
    (Ctx.add gamma (DeclareExistential alphaEx k))
    e
    (substitute a (UniversalVariable alpha) (ExistentialVariable alphaEx), Nonprincipal)
  pure delta
-- RULE: ⊃I (Implies introduction)
-- RULE: ⊃I⊥ (Implies introduction bottom)
check' gamma v (Implies p a, Principal) = do
  unless (checkedIntroduction v) $ throwError AnalysisError
  marker <- Marker <$> freshExistential
  catchError
    (do
      theta <- Equation.incorporate (Ctx.add gamma marker) p
      ctx   <- check theta v (Ctx.apply theta a, Principal)
      pure $ Ctx.drop ctx marker
    )
    (const $ pure gamma)
-- RULE: ∧I (With introduction)
check' gamma e (With a prop, p) = do
  when
      (case e of
        Syntax.Match _ _ _ -> True
        _                  -> False
      )
    $ throwError AnalysisError
  theta <- Equation.true gamma prop
  check theta e (Ctx.apply theta a, p)
-- RULE: →I (Function introduction)
check' gamma (Syntax.Lambda _ x e) (Function a b, p) = do
  let binding = Binding x a p
      gamma'  = Ctx.add gamma binding
  ctx <- check gamma' e (b, p)
  pure $ Ctx.drop ctx binding
-- RULE: →Iα̂ (Function introduction existential)
check' gamma (Syntax.Lambda _ x e) (ExistentialVariable alpha, Nonprincipal)
  | (alpha, Type) `Set.member` (Ctx.existentials gamma) = do
    alpha1 <- freshExistential
    alpha2 <- freshExistential
    let marker       = Marker alpha1
        binding      = Binding x (ExistentialVariable alpha1) Nonprincipal
        declarations = [DeclareExistential alpha1 Type, DeclareExistential alpha2 Type]
        (pre, post)  = Ctx.split gamma (DeclareExistential alpha Type)
        gamma'       = Ctx.add (Ctx.splice pre (marker : declarations) post) binding
    theta <- check gamma' e (ExistentialVariable alpha2, Nonprincipal)
    let (delta, delta') = Ctx.split theta marker
        unsolvedExistentials =
          reverse [ (tv, kind) | DeclareExistential tv kind <- toList $ unContext delta' ]
    replacementUniversals <- forM unsolvedExistentials $ \(_, kind) -> (, kind) <$> freshUniversal
    let
      replacementDeclarations = map (uncurry DeclareUniversal) replacementUniversals
      replacementContext      = Context $ Seq.fromList $ zipWith
        (\(evar, kind) (uvar, _) -> SolvedExistential evar kind (UniversalVariable uvar))
        unsolvedExistentials
        replacementUniversals
      function = Ctx.apply replacementContext $ Ctx.apply delta' $ Function
        (ExistentialVariable alpha1)
        (ExistentialVariable alpha2)
      generalizedTau =
        foldl' (\typ (uvar, kind) -> Forall uvar kind typ) function replacementUniversals
    pure $ Ctx.adds delta $ replacementDeclarations <> [SolvedExistential alpha Type generalizedTau]
-- RULE: Case
check' gamma (Syntax.Match _ e pi) (c, p) = do
  ((a, q), theta) <- synthesize gamma e
  delta           <- Match.check theta pi ([Ctx.apply theta a], q) (Ctx.apply theta c, p)
  unless (Match.covers delta pi ([Ctx.apply delta a], q)) $ throwError InsufficientCoverage
  pure delta
-- RULE: +Iₖ (Sum injection introduction)
check' gamma (Syntax.Variant _ k e) (Variant aMap, p) = do
  ak <- liftMaybe AnalysisError $ Map.lookup k aMap
  check gamma e (ak, p)
-- RULE: +Iâₖ (Sum injection introduction existential)
check' gamma term@(Syntax.Variant _ k e) (ExistentialVariable alpha, Nonprincipal) = do
  vMap <- sequenceA $ Map.singleton k freshExistential
  let (pre, post) = Ctx.split gamma (DeclareExistential alpha Type)
      variant     = Variant $ map ExistentialVariable vMap
      ctx         = Ctx.splice
        pre
        (  (map (\exvar -> DeclareExistential exvar Type) $ elems vMap)
        <> [SolvedExistential alpha Type variant]
        )
        post
  check ctx term (variant, Nonprincipal)
-- RULE: ×I (Product introduction)
check' gamma (Syntax.Tuple _ eMap) (Tuple aMap, p) = do
  let missing = Map.traverseMissing $ \_ _ -> throwError AnalysisError
      tuple   = Map.zipWithAMatched $ \_ e a -> pure (e, a)
  eaList <- Map.elems <$> Map.mergeA missing missing tuple eMap aMap
  foldM (\ctx (en, an) -> check ctx en (Ctx.apply ctx an, p)) gamma eaList
check' gamma (Syntax.Record _ eMap) (Record aMap, p) = do
  let missing = Map.traverseMissing $ \_ _ -> throwError AnalysisError
      tuple   = Map.zipWithAMatched $ \_ e a -> pure (e, a)
  eaList <- Map.elems <$> Map.mergeA missing missing tuple eMap aMap
  foldM (\ctx (en, an) -> check ctx en (Ctx.apply ctx an, p)) gamma eaList
-- RULE: ×Iâₖ (Product introduction existential)
check' gamma term@(Syntax.Tuple _ eMap) (ExistentialVariable alpha, Nonprincipal) = do
  vMap <- forM eMap $ const freshExistential
  let (pre, post) = Ctx.split gamma (DeclareExistential alpha Type)
      tuple       = Tuple $ map ExistentialVariable vMap
      ctx         = Ctx.splice
        pre
        (  (map (\exvar -> DeclareExistential exvar Type) $ elems vMap)
        <> [SolvedExistential alpha Type tuple]
        )
        post
  check ctx term (tuple, Nonprincipal)
check' gamma term@(Syntax.Record _ eMap) (ExistentialVariable alpha, Nonprincipal) = do
  vMap <- forM eMap $ const freshExistential
  let (pre, post) = Ctx.split gamma (DeclareExistential alpha Type)
      record      = Record $ map ExistentialVariable vMap
      ctx         = Ctx.splice
        pre
        (  (map (\exvar -> DeclareExistential exvar Type) $ elems vMap)
        <> [SolvedExistential alpha Type record]
        )
        post
  check ctx term (record, Nonprincipal)
-- RULE: Nil
check' gamma (Syntax.Vector _ []       ) (Vector t _, _) = Equation.true gamma (Equals t Zero)
-- RULE: Cons
check' gamma (Syntax.Vector _ (e1 : e2)) (Vector t a, p) = do
  alpha <- freshExistential
  let alphaType = ExistentialVariable alpha
      marker    = Marker alpha
  gamma' <- Equation.true (Ctx.adds gamma [marker, DeclareExistential alpha Natural])
                          (Equals t $ Succ alphaType)
  theta <- check gamma' e1 (Ctx.apply gamma' a, p)
  ctx   <- check theta (Syntax.Vector () e2) (Ctx.apply theta $ Vector alphaType a, Nonprincipal)
  pure $ Ctx.drop ctx marker
-- RULE: Sub
check' gamma expr (b, _) = do
  ((a, _), theta) <- synthesize gamma expr
  let polarity = (Sub.join `on` Sub.polarity) b a
  subtype theta polarity a b

atomType :: Syntax.Atom -> Type
atomType = Primitive . \case
  Syntax.Unit      -> "Unit"
  Syntax.Integer _ -> "Integer"
  Syntax.String  _ -> "String"
  Syntax.Boolean _ -> "Boolean"

checkedIntroduction :: Term -> Bool
checkedIntroduction (Syntax.Lambda _ _ _ ) = True
checkedIntroduction (Syntax.Atom   _ _   ) = True
checkedIntroduction (Syntax.Record _ _   ) = True
checkedIntroduction (Syntax.Variant _ _ _) = True
checkedIntroduction (Syntax.Vector _ _   ) = True
checkedIntroduction _                      = False
