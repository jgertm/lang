{-|
This module implements the type __CHECKING__ (@<=@) rules from /figure 14a:: Algorithmic typing, including rules omitted from main paper/ (page 37)
-}
module Type.Analysis
  ( check
  )
where

import qualified Data.Map.Merge.Strict         as Map
import qualified Data.Map.Strict               as Map
import qualified Data.Sequence                 as Seq
import qualified Data.Set                      as Set
import           Data.Text.Prettyprint.Doc

import           Error                          ( TypeError(..) )
import qualified Syntax.Common                 as Syntax
import qualified Syntax.Term                   as Term
import qualified Type.Context                  as Ctx
import qualified Type.Equation                 as Equation
import           Type.Expression
import {-# SOURCE #-} qualified Type.Match     as Match
import           Type.Monad
import           Type.Subtyping                 ( subtype )
import qualified Type.Subtyping                as Sub
import {-# SOURCE #-} Type.Synthesis            ( synthesize )
import {-# SOURCE #-} qualified Type.Synthesis as Synth
import           Type.Types


check, check' :: Context -> Term -> (Type, Principality) -> Infer Context

check = check'

check' gamma term (ExistentialVariable alpha, Nonprincipal)
  | alpha `Map.member` Ctx.existentialSolutions gamma
  = case Map.lookup alpha (Ctx.existentialSolutions gamma) of
    Just (solution, Type) -> check gamma term (solution, Nonprincipal)
    Just (_       , kind) -> typeerror $ KindMismatch Type kind
    Nothing               -> typeerror $ UnsolvedExistential alpha
-- TODO: equirecursive vs isorecursive
check' gamma term (typ@(Fix alpha sub), p) = do
  let (pre, post) = Ctx.split gamma (DeclareExistential alpha Type)
      solution    = SolvedExistential alpha Type typ
      gamma'      = Ctx.inject pre solution post
  -- gamma'' <- check gamma' term (sub, p) -- FIXME
  pure $ gamma'
-- RULE: Rec
check' gamma (Term.Fix _ x v) ap@(a, p) = do
  let binding = Binding x a p
      gamma'  = Ctx.add gamma binding
  gamma'' <- check gamma' v ap
  pure $ Ctx.drop gamma'' binding
-- RULE: 1Iα̂  (Atom introduction existential)
check' gamma (Term.Atom _ atom) (ExistentialVariable alpha, Nonprincipal)
  | (alpha, Type) `Set.member` Ctx.existentials gamma = do
    let typ         = Synth.atom atom
        (pre, post) = Ctx.split gamma (DeclareExistential alpha Type)
    pure $ Ctx.inject pre (SolvedExistential alpha Type typ) post
-- RULE: 1I (Atom introduction)
check' gamma (Term.Atom _ atom) (typ, _) = do
  let typ' = Synth.atom atom
  unless (typ == typ') $ typeerror (TypeMismatch typ typ')
  pure gamma
-- RULE: Let [2013 paper]
check' gamma (Term.Let _ bindings body) cp = do
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
check' gamma (Term.If _ test true false) ap = do
  theta <- check gamma test (boolean, Principal)
  foldM (\ctx branch -> check ctx branch ap) theta [true, false]
-- RULE: ∀I (Forall introduction)
check' gamma v (Forall alpha k a, p) = do
  unless (checkedIntroduction v) $ typeerror AnalysisError
  let declaration = DeclareUniversal alpha k
  gamma' <- check (Ctx.add gamma declaration) v (a, p)
  pure $ Ctx.drop gamma' declaration
-- RULE: ∃I (Exists introduction)
check' gamma e (Exists alpha k a, _) = do
  unless (checkedIntroduction e) $ typeerror AnalysisError
  alphaEx <- freshExistential
  let gamma' = Ctx.add gamma (DeclareExistential alphaEx k)
      a'     = substitute a (UniversalVariable alpha) (ExistentialVariable alphaEx)
  check gamma' e (a', Nonprincipal)
-- RULE: ⊃I (Implies introduction)
-- RULE: ⊃I⊥ (Implies introduction bottom)
check' gamma v (Implies p a, Principal) = do
  unless (checkedIntroduction v) $ typeerror AnalysisError
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
        Term.Match{} -> True
        _            -> False
      )
    $ typeerror AnalysisError
  theta <- Equation.true gamma prop
  check theta e (Ctx.apply theta a, p)
-- RULE: →I (Function introduction)
check' gamma (Term.Lambda _ arg e) (Function a b, p) = do
  let binding = Binding arg a p
      gamma'  = Ctx.add gamma binding
  ctx <- check gamma' e (b, p)
  pure $ Ctx.drop ctx binding
-- RULE: →Iα̂ (Function introduction existential)
check' gamma (Term.Lambda _ arg e) (alphaType@(ExistentialVariable alpha), Nonprincipal)
  | (alpha, Type) `Set.member` Ctx.existentials gamma = do
    alpha1 <- freshExistential
    alpha2 <- freshExistential
    let
      marker       = Marker alpha1
      binding      = Binding arg (ExistentialVariable alpha1) Nonprincipal
      declarations = [DeclareExistential alpha1 Type, DeclareExistential alpha2 Type]
      (pre, post)  = Ctx.split gamma (DeclareExistential alpha Type)
      function     = Function (ExistentialVariable alpha1) (ExistentialVariable alpha2)
      gamma'       = Ctx.add
        (Ctx.splice pre (marker : declarations) (Ctx.substitute post alphaType function))
        binding
    theta <- check gamma' e (ExistentialVariable alpha2, Nonprincipal)
    let (delta, delta') = Ctx.split theta marker
    (facts, uvars) <- runWriterT $ flip concatMapM (Seq.reverse $ unContext delta') $ \case
      DeclareExistential evar kind -> do
        uvar <- lift freshUniversal
        tell [(uvar, kind)]
        pure [DeclareUniversal uvar kind, SolvedExistential evar kind (UniversalVariable uvar)]
      fact -> pure [fact]
    let delta'' = Context $ Seq.reverse facts
        generalizedFunction =
          foldl' (\typ (uvar, kind) -> Forall uvar kind typ) (Ctx.apply delta'' function) uvars
    pure $ Ctx.inject delta (SolvedExistential alpha Type generalizedFunction) delta''
-- RULE: Case
check' gamma (Term.Match _ e pi) (c, p) = do
  let expand Term.Branch { patterns, body } =
        map (\pat -> Term.Branch {patterns = [pat], body }) patterns
      pi' = concatMap expand pi
  ((a, q), theta) <- synthesize gamma e
  delta           <- Match.check theta pi' ([Ctx.apply theta a], q) (Ctx.apply theta c, p)
  unless (Match.covers delta pi' ([Ctx.apply delta a], q)) $ typeerror InsufficientCoverage
  pure delta
-- RULE: +Iₖ (Sum injection introduction)
check' gamma (Term.Variant _ extent k e) (Variant row aMap, p)
  | compareExtent extent row && (p == Principal || k `Map.member` aMap) = do
    let ak =
          fromMaybe (error $ show $ "[type.analysis] couldn't find tag: " <> pretty k)
            $ Map.lookup k aMap
    check gamma e (ak, p)
-- RULE: +Iâₖ (Sum injection introduction existential)
check' gamma (Term.Variant _ Syntax.Open k e) (variant@(Variant (Open rowvar) aMap), Nonprincipal)
  = do
    alpha <- freshExistential
    let
      (pre, post) = Ctx.split gamma (SolvedExistential rowvar Type variant)
      ak          = ExistentialVariable alpha
      variant'    = Variant (Open rowvar) $ Map.insert k ak aMap
      ctx =
        Ctx.splice pre [DeclareExistential alpha Type, SolvedExistential rowvar Type variant'] post
    check ctx e (ak, Nonprincipal)
check' gamma term@(Term.Variant _ Syntax.Open k _) (ExistentialVariable alpha, Nonprincipal)
  | alpha `Map.notMember` Ctx.existentialSolutions gamma = do
    ak <- freshExistential
    let
      (pre, post) = Ctx.split gamma (DeclareExistential alpha Type)
      variant = Variant (Open alpha) $ Map.singleton k (ExistentialVariable ak)
      ctx = Ctx.splice pre [DeclareExistential ak Type, SolvedExistential alpha Type variant] post
    check ctx term (variant, Nonprincipal)
check' gamma term@(Term.Variant _ Syntax.Closed k _) (ExistentialVariable alpha, Nonprincipal)
  | alpha `Map.notMember` Ctx.existentialSolutions gamma = do
    typedefs <- ask
    let variant =
          fromMaybe (error "[type.analysis] couldn't find defined variant type for tag")
            $ findVariant k typedefs
        gamma' = gamma
    check gamma' term (variant, Principal)
-- RULE: ×I (Product introduction)
check' gamma (Term.Tuple _ eMap) (Tuple aMap, p) = do
  let missing = Map.traverseMissing $ \_ _ -> typeerror AnalysisError
      tuple   = Map.zipWithMatched $ \_ e a -> (e, a)
  eaList <- elems <$> Map.mergeA missing missing tuple eMap aMap
  foldM (\ctx (en, an) -> check ctx en (Ctx.apply ctx an, p)) gamma eaList
check' gamma (Term.Record _ Syntax.Closed eMap) (Record Closed aMap, p) = do
  let missing = Map.traverseMissing $ \_ _ -> typeerror AnalysisError
      tuple   = Map.zipWithMatched $ \_ e a -> (e, a)
      recur ctx (en, an) = check ctx en (Ctx.apply ctx an, p)
  eaMap <- Map.mergeA missing missing tuple eMap aMap
  foldM recur gamma $ elems eaMap
-- RULE: ×Iâₖ (Product introduction existential)
check' gamma term@(Term.Tuple _ eMap) (ExistentialVariable alpha, Nonprincipal) = do
  vMap <- forM eMap $ const freshExistential
  let (pre, post) = Ctx.split gamma (DeclareExistential alpha Type)
      tuple       = Tuple $ map ExistentialVariable vMap
      ctx         = Ctx.splice
        pre
        (map (`DeclareExistential` Type) (elems vMap) <> [SolvedExistential alpha Type tuple])
        post
  check ctx term (tuple, Nonprincipal)
check' gamma term@(Term.Record _ extent eMap) (ExistentialVariable alpha, Nonprincipal) = do
  vMap <- forM eMap $ const freshExistential
  let (pre, post) = Ctx.split gamma (DeclareExistential alpha Type)
      row         = case extent of
        Syntax.Open   -> Open alpha
        Syntax.Closed -> Closed
      record = Record row $ map ExistentialVariable vMap
      ctx    = Ctx.splice
        pre
        (map (`DeclareExistential` Type) (elems vMap) <> [SolvedExistential alpha Type record])
        post
  check ctx term (record, Nonprincipal)
-- RULE: Nil
check' gamma (Term.Vector _ []       ) (Vector t _, _) = Equation.true gamma (Equals t Zero)
-- RULE: Cons
check' gamma (Term.Vector _ (e1 : e2)) (Vector t a, p) = do
  alpha <- freshExistential
  let alphaType = ExistentialVariable alpha
      marker    = Marker alpha
  gamma' <- Equation.true (Ctx.adds gamma [marker, DeclareExistential alpha Natural])
                          (Equals t $ Succ alphaType)
  theta <- check gamma' e1 (Ctx.apply gamma' a, p)
  ctx   <- check theta (Term.Vector () e2) (Ctx.apply theta $ Vector alphaType a, Nonprincipal)
  pure $ Ctx.drop ctx marker
-- RULE: Sub
check' gamma expr (b, _) = do
  ((a, _), theta) <- synthesize gamma expr
  let polarity = (Sub.join `on` Sub.polarity) b a
  subtype theta polarity a b

checkedIntroduction :: Term -> Bool
checkedIntroduction Term.Lambda{}  = True
checkedIntroduction Term.Atom{}    = True
checkedIntroduction Term.Record{}  = True
checkedIntroduction Term.Variant{} = True
checkedIntroduction Term.Vector{}  = True
checkedIntroduction _              = False
