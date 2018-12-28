module Type.Match where

import qualified Data.Map.Merge.Strict         as Map
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set

import           Error                          ( TypeError(..) )
import qualified Syntax.Pattern                as Pattern
import qualified Syntax.Reference              as Syntax
import qualified Type.Analysis                 as Analysis
import qualified Type.Context                  as Ctx
import qualified Type.Equation                 as Equation
import           Type.Expression
import           Type.Monad
import           Type.Types


check :: Context -> [Branch] -> ([Type], Principality) -> (Type, Principality) -> Infer Context
-- RULE: MatchEmpty
check gamma []        _       _      = pure gamma
-- RULE: MatchBase
check gamma [([], e)] ([], _) (c, p) = Analysis.check gamma e (c, p)
-- RULE: MatchUnit
check gamma [(Pattern.Atom _ atom : rhos, e)] (primitive : as, q) cp =
  let typ = Analysis.atomType atom
  in  if primitive == typ
        then check gamma [(rhos, e)] (as, q) cp
        else throwError $ TypeMismatch primitive typ
-- RULE: Match∃
check gamma branches (Exists alpha k a : as, q) cp =
  check (Ctx.add gamma (DeclareUniversal alpha k)) branches (a : as, q) cp
-- RULE: Match∧
check gamma branches (With a p : as, Principal) cp =
  incorporate gamma p branches (a : as, Principal) cp
-- RULE: Match∧!/ (Nonprincipal)
check gamma branches (With a _ : as, Nonprincipal) cp =
  check gamma branches (a : as, Nonprincipal) cp
-- RULE: Match×
check gamma [(Pattern.Tuple _ rhoMap : rhos, e)] (Tuple aMap : as, q) cp
  | Map.keysSet rhoMap == Map.keysSet aMap = do
    let tuplePatterns = elems rhoMap
        tupleTypes    = elems aMap
    check gamma [(tuplePatterns <> rhos, e)] (tupleTypes <> as, q) cp
check gamma [(Pattern.Record _ rhoMap : rhos, e)] (Record aMap : as, q) cp
  | Map.keysSet rhoMap `Set.isSubsetOf` Map.keysSet aMap = do
    let rowAs          = Map.restrictKeys aMap (Map.keysSet rhoMap)
        recordPatterns = elems rhoMap
        recordTypes    = elems rowAs
    check gamma [(recordPatterns <> rhos, e)] (recordTypes <> as, q) cp
-- RULE: Match+ₖ
check gamma [(Pattern.Variant _ tag rho : rhos, e)] (Variant aMap : as, q) cp = do
  a <- liftMaybe MatchError $ Map.lookup tag aMap
  check gamma [(rho : rhos, e)] (a : as, q) cp
-- RULE: MatchNeg
check gamma [(Pattern.Symbol _ z : rhos, e)] (a : as, q) cp
  | not $ isWith a || isExistentiallyQuantified a = do
    let binding = Binding z a Principal
    ctx <- check (Ctx.add gamma binding) [(rhos, e)] (as, q) cp
    pure $ Ctx.drop ctx binding
-- RULE: MatchWild
check gamma [(Pattern.Wildcard _ : rhos, e)] (a : as, q) cp
  | not $ isWith a || isExistentiallyQuantified a = check gamma [(rhos, e)] (as, q) cp
-- RULE: MatchNil
check gamma [(Pattern.Vector _ [] : rhos, e)] (Vector t _ : as, Principal) cp =
  incorporate gamma (Equals t Zero) [(rhos, e)] (as, Principal) cp
-- RULE: MatchCons
check gamma [(Pattern.Vector _ (rho1 : rho2) : rhos, e)] (Vector t a : as, Principal) cp = do
  alpha <- freshUniversal
  let alphaType   = UniversalVariable alpha
      declaration = DeclareUniversal alpha Natural
      gamma'      = Ctx.add gamma declaration
  ctx <- incorporate gamma'
                     (Equals t (Succ alphaType))
                     [(rho1 : Pattern.Vector () rho2 : rhos, e)]
                     (a : Vector alphaType a : as, Principal)
                     cp
  pure $ Ctx.drop ctx declaration
-- RULE: MatchNil!/ (Nonprincipal)
check gamma [(Pattern.Vector _ [] : rhos, e)] (Vector _ _ : as, Nonprincipal) cp =
  check gamma [(rhos, e)] (as, Nonprincipal) cp
-- RULE: MatchCons!/ (Nonprincipal)
check gamma [(Pattern.Vector _ (rho1 : rho2) : rhos, e)] (Vector _ a : as, Nonprincipal) cp = do
  alpha <- freshUniversal
  let alphaType   = UniversalVariable alpha
      declaration = DeclareUniversal alpha Natural
      gamma'      = Ctx.add gamma declaration
  ctx <- check gamma'
               [(rho1 : Pattern.Vector () rho2 : rhos, e)]
               (a : Vector alphaType a : as, Nonprincipal)
               cp
  pure $ Ctx.drop ctx declaration
-- RULE: MatchSeq
check gamma (pi : pi') (as, q) cp = do
  theta <- check gamma [pi] (as, q) cp
  check theta pi' (map (Ctx.apply theta) as, q) cp

incorporate
  :: Context
  -> Proposition
  -> [Branch]
  -> ([Type], Principality)
  -> (Type, Principality)
  -> Infer Context
-- RULE: Match⊥
-- RULE: MatchUnify
incorporate gamma (Equals sigma tau) [(rhos, e)] (as, Principal) cp = do
  p <- freshExistential
  let marker = Marker p
  catchError
    (do
      theta <- Equation.unify (Ctx.add gamma marker) sigma (tau, Type) -- FIXME: kind is unspecified
      ctx   <- check theta [(rhos, e)] (as, Principal) cp
      pure $ Ctx.drop ctx marker
    )
    (const $ pure gamma)
incorporate _ _ _ _ _ = throwError MatchError

covers :: Context -> [Branch] -> ([Type], Principality) -> Bool
-- RULE: CoversEmpty
covers _ (([], _) : _) ([], _) = True
-- RULE: Covers1 (Atom)
covers gamma pis (Primitive _ : as, q) = let pis' = expandAtom pis in covers gamma pis' (as, q)
-- RULE: Covers× (Tuple/Record)
covers gamma pis (Tuple aMap : as, q) =
  let pis' = expandTuple pis in covers gamma pis' (elems aMap <> as, q)
covers gamma pis (Record aMap : as, q) =
  let pis' = expandRecord pis in covers gamma pis' (elems aMap <> as, q)
-- RULE: Covers+ (Variant)
covers gamma pis (Variant aMap : as, q) =
  let pisMap  = expandVariant pis
      missing = Map.traverseMissing $ \_ _ -> Nothing
      tuple   = Map.zipWithAMatched $ \_ branches a -> Just (branches, a)
  in  case Map.mergeA missing missing tuple pisMap aMap of
        Nothing      -> False
        Just pisAMap -> all (\(pisK, aK) -> covers gamma pisK (aK : as, q)) pisAMap
-- RULE: Covers∃
covers gamma pis (Exists alpha k _ : as, q) =
  let gamma' = Ctx.add gamma $ DeclareUniversal alpha k in covers gamma' pis (as, q)
-- RULE: Covers∧
covers gamma pis (With a0 prop : as, Principal) =
  coversAssuming gamma prop pis (a0 : as, Principal)
-- RULE: Covers∧!/ (Nonprincipal)
covers gamma pis (With a0 _ : as, Nonprincipal) = covers gamma pis (a0 : as, Nonprincipal) -- FIXME: I think there's a typo regarding this rule in the paper
-- TODO: CoversVec
-- TODO: CoversVec!/ (Nonprincipal)
-- RULE: CoversVar
covers gamma pis (_ : as, q) = let pis' = expandVariable pis in covers gamma pis' (as, q)
covers _ _ _ = False
-- TODO: CoversEq
-- TODO: CoversEqBot

coversAssuming :: Context -> [Branch] -> Proposition -> ([Type], Principality) -> Infer ()
coversAssuming = undefined

guarded :: [Branch] -> Bool
guarded ((Pattern.Vector _ [] : _, _) : _) = True
guarded ((Pattern.Vector _ (_ : _) : _, _) : _) = True
guarded ((Pattern.Wildcard _ : _, _) : pis) = guarded pis
guarded ((Pattern.Symbol _ _ : _, _) : pis) = guarded pis
guarded _ = False

expandVector :: [Branch] -> ([Branch], [Branch])
expandVector [] = ([], [])
expandVector ((rho : rhos, e) : pis) | isSymbol rho || isWildcard rho =
  let (pisNil, pisCons) = expandVector pis
      left              = (rhos, e) : pisNil
      right             = (Pattern.Wildcard () : Pattern.Wildcard () : rhos, e) : pisCons
  in  (left, right)
expandVector ((Pattern.Vector _ [] : rhos, e) : pis) =
  let (pisNil, pisCons) = expandVector pis in ((rhos, e) : pisNil, pisCons)
expandVector ((Pattern.Vector _ (rho : rho') : rhos, e) : pis) =
  let (pisNil, pisCons) = expandVector pis in (pisNil, ((rho : rho') <> rhos, e) : pisCons)
expandVector _ = error "Can only expand vector pattern"

expandTuple :: [Branch] -> [Branch]
expandTuple [] = []
expandTuple ((Pattern.Tuple _ rhoMap : rhos, e) : pis) =
  let pis' = expandTuple pis in (elems rhoMap <> rhos, e) : pis'
expandTuple ((rho : rhos, e) : pis) | isSymbol rho || isWildcard rho =
  let pis' = expandTuple pis in ((Pattern.Wildcard () : Pattern.Wildcard () : rhos, e) : pis')
expandTuple _ = error "Can only expand tuple pattern"

expandRecord :: [Branch] -> [Branch]
expandRecord [] = []
expandRecord ((Pattern.Record _ rhoMap : rhos, e) : pis) =
  let pis' = expandRecord pis in (elems rhoMap <> rhos, e) : pis'
expandRecord ((rho : rhos, e) : pis) | isSymbol rho || isWildcard rho =
  let pis' = expandRecord pis in ((Pattern.Wildcard () : Pattern.Wildcard () : rhos, e) : pis')
expandRecord _ = error "Can only expand record pattern"

expandVariant :: [Branch] -> Map Syntax.Name [Branch]
expandVariant [] = mempty
expandVariant ((Pattern.Variant _ k rho : rhos, e) : pis) =
  let pisKs = expandVariant pis in Map.adjust ((rho : rhos, e) :) k pisKs
expandVariant ((rho : rhos, e) : pis) | isSymbol rho || isWildcard rho =
  let pisKs      = expandVariant pis
      commonHead = (Pattern.Wildcard () : rhos, e)
  in  map (commonHead :) pisKs
expandVariant _ = error "Can only expand variant pattern"

expandVariable :: [Branch] -> [Branch]
expandVariable [] = []
expandVariable ((rho : rhos, e) : pis) | isSymbol rho || isWildcard rho =
  let pis' = expandVariable pis in (rhos, e) : pis'
expandVariable _ = error "Can only expand variable or wildcard pattern"

expandAtom :: [Branch] -> [Branch]
expandAtom [] = []
expandAtom ((rho : rhos, e) : pis) | isSymbol rho || isWildcard rho || isAtom rho =
  let pis' = expandAtom pis in (rhos, e) : pis'
expandAtom _ = error "Can only expand variable, wildcard or atom pattern"


isSymbol, isWildcard, isAtom :: Pattern -> Bool

isSymbol (Pattern.Symbol _ _) = True
isSymbol _                    = False

isWildcard (Pattern.Wildcard _) = True
isWildcard _                    = False

isAtom (Pattern.Atom _ _) = True
isAtom _                  = False
