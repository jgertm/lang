module Type.Match
  ( check
  , covers
  , expandVariant
  )
where

import qualified Data.Map.Merge.Strict         as Map
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Data.Text.Prettyprint.Doc

import           Error                          ( TypeError(..) )
import qualified Syntax.Pattern                as Pattern
import qualified Syntax.Reference              as Syntax
import qualified Syntax.Term                   as Term
import qualified Type.Analysis                 as Analysis
import qualified Type.Context                  as Ctx
import qualified Type.Equation                 as Equation
import           Type.Expression
import qualified Type.Instantiation            as Instantiate
import           Type.Monad
import qualified Type.Synthesis                as Synth
import           Type.Types


check :: Context -> [Branch] -> ([Type], Principality) -> (Type, Principality) -> Infer Context
-- RULE: MatchEmpty
check gamma [] _ _ = pure gamma
-- RULE: MatchBase
check gamma [Term.Branch { patterns = [], body = e }] ([], _) (c, p) =
  Analysis.check gamma e (c, p)
-- RULE: MatchUnit
check gamma [Term.Branch { patterns = Pattern.Atom _ atom : rhos, body = e }] ap@(primitive : as, q) cp
  = do
    let typ = Synth.atom atom
    gamma' <- if primitive == typ
      then pure gamma
      else catchError (Instantiate.to gamma primitive (typ, Type))
                      (const $ throwError $ TypeMismatch primitive typ)
    check gamma' [Term.Branch {patterns = rhos, body = e}] (map (Ctx.apply gamma') as, q) cp
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
check gamma [Term.Branch { patterns = Pattern.Tuple _ rhoMap : rhos, body = e }] (Tuple aMap : as, q) cp
  | Map.keysSet rhoMap == Map.keysSet aMap
  = let tuplePatterns = elems rhoMap
        tupleTypes    = elems aMap
    in  check gamma
              [Term.Branch {patterns = tuplePatterns <> rhos, body = e}]
              (tupleTypes <> as, q)
              cp
check gamma branches@[Term.Branch { patterns = Pattern.Tuple _ rhoMap : _ }] (ExistentialVariable alpha : as, Nonprincipal) cp
  = do
    vMap <- forM rhoMap $ const freshExistential
    let (pre, post) = Ctx.split gamma (DeclareExistential alpha Type)
        tuple       = Tuple $ map ExistentialVariable vMap
        ctx         = Ctx.splice
          pre
          (map (`DeclareExistential` Type) (elems vMap) <> [SolvedExistential alpha Type tuple])
          post
    check ctx branches (tuple : as, Nonprincipal) cp
check gamma [Term.Branch { patterns = Pattern.Record _ rhoMap : rhos, body = e }] (Record aMap : as, q) cp
  | Map.keysSet rhoMap `Set.isSubsetOf` Map.keysSet aMap
  = do
    let rowAs          = Map.restrictKeys aMap (Map.keysSet rhoMap)
        recordPatterns = elems rhoMap
        recordTypes    = elems rowAs
    check gamma
          [Term.Branch {patterns = recordPatterns <> rhos, body = e}]
          (recordTypes <> as, q)
          cp
-- RULE: Match+ₖ
check gamma [Term.Branch { patterns = Pattern.Variant _ k rho : rhos, body = e }] (Variant _ aMap : as, q) cp
  | q == Principal || k `Map.member` aMap
  = let
      a = fromMaybe (error $ show $ "[type.match] couldn't find tag: " <> pretty k)
        $ Map.lookup k aMap
    in  check gamma [Term.Branch {patterns = rho : rhos, body = e}] (a : as, q) cp
check gamma branches@[Term.Branch { patterns = Pattern.Variant _ k rho : _ }] (variant@(Variant (Just rowvar) aMap) : as, Nonprincipal) cp
  = do
    ak <- freshExistential
    let
      (pre, post) = Ctx.split gamma (SolvedExistential rowvar Type variant)
      variant'    = Variant (Just rowvar) $ Map.insert k (ExistentialVariable ak) aMap
      ctx =
        Ctx.splice pre [DeclareExistential ak Type, SolvedExistential rowvar Type variant'] post
    check ctx branches (variant' : as, Nonprincipal) cp
check gamma branches@[Term.Branch { patterns = Pattern.Variant _ k rho : rhos, body = e }] (ExistentialVariable alpha : as, Nonprincipal) cp
  = do
    ak <- freshExistential
    let
      (pre, post) = Ctx.split gamma (DeclareExistential alpha Type)
      variant = Variant (Just alpha) $ Map.singleton k (ExistentialVariable ak)
      ctx = Ctx.splice pre [DeclareExistential ak Type, SolvedExistential alpha Type variant] post
    check ctx branches (variant : as, Nonprincipal) cp
-- RULE: MatchNeg
check gamma [Term.Branch { patterns = Pattern.Symbol _ z : rhos, body = e }] (a : as, q) cp
  | not $ isWith a || isExistentiallyQuantified a = do
    let binding = Binding z a Principal
    ctx <- check (Ctx.add gamma binding) [Term.Branch {patterns = rhos, body = e}] (as, q) cp
    pure $ Ctx.drop ctx binding
-- RULE: MatchWild
check gamma [Term.Branch { patterns = Pattern.Wildcard _ : rhos, body = e }] (a : as, q) cp
  | not $ isWith a || isExistentiallyQuantified a = check
    gamma
    [Term.Branch {patterns = rhos, body = e}]
    (as, q)
    cp
-- RULE: MatchNil
check gamma [Term.Branch { patterns = Pattern.Vector _ [] : rhos, body = e }] (Vector t _ : as, Principal) cp
  = incorporate gamma (Equals t Zero) [Term.Branch {patterns = rhos, body = e}] (as, Principal) cp
-- RULE: MatchCons
check gamma [Term.Branch { patterns = Pattern.Vector _ (rho1 : rho2) : rhos, body = e }] (Vector t a : as, Principal) cp
  = do
    alpha <- freshUniversal
    let alphaType   = UniversalVariable alpha
        declaration = DeclareUniversal alpha Natural
        gamma'      = Ctx.add gamma declaration
    ctx <- incorporate gamma'
                       (Equals t (Succ alphaType))
                       [Term.Branch {patterns = rho1 : Pattern.Vector () rho2 : rhos, body = e}]
                       (a : Vector alphaType a : as, Principal)
                       cp
    pure $ Ctx.drop ctx declaration
-- RULE: MatchNil!/ (Nonprincipal)
check gamma [Term.Branch { patterns = Pattern.Vector _ [] : rhos, body = e }] (Vector _ _ : as, Nonprincipal) cp
  = check gamma [Term.Branch {patterns = rhos, body = e}] (as, Nonprincipal) cp
-- RULE: MatchCons!/ (Nonprincipal)
check gamma [Term.Branch { patterns = Pattern.Vector _ (rho1 : rho2) : rhos, body = e }] (Vector _ a : as, Nonprincipal) cp
  = do
    alpha <- freshUniversal
    let alphaType   = UniversalVariable alpha
        declaration = DeclareUniversal alpha Natural
        gamma'      = Ctx.add gamma declaration
    ctx <- check gamma'
                 [Term.Branch {patterns = rho1 : Pattern.Vector () rho2 : rhos, body = e}]
                 (a : Vector alphaType a : as, Nonprincipal)
                 cp
    pure $ Ctx.drop ctx declaration
-- RULE: MatchSeq
check gamma (pi@Term.Branch { patterns } : pi') ([a], p) cp = do
  let as = replicate (length patterns) a
  theta <- check gamma [pi] (as, p) cp
  check theta pi' (map (Ctx.apply theta) [a], p) cp
check gamma pis@(pi : pi') (as, q) cp =
  foldM (\ctx pi -> check ctx [pi] (map (Ctx.apply ctx) as, q) cp) gamma pis

incorporate
  :: Context
  -> Proposition
  -> [Branch]
  -> ([Type], Principality)
  -> (Type, Principality)
  -> Infer Context
-- RULE: Match⊥
-- RULE: MatchUnify
incorporate gamma (Equals sigma tau) branches (as, Principal) cp = do
  p <- freshExistential
  let marker = Marker p
  catchError
    (do
      theta <- Equation.unify (Ctx.add gamma marker) sigma (tau, Type) -- FIXME: kind is unspecified
      ctx   <- check theta branches (as, Principal) cp
      pure $ Ctx.drop ctx marker
    )
    (const $ pure gamma)
incorporate _ _ _ _ _ = error "incorporate fallthrough"

covers, covers' :: Context -> [Branch] -> ([Type], Principality) -> Bool

covers = covers'

-- RULE: CoversEmpty
covers' _ (Term.Branch { patterns = [] } : _) ([], _) = True
-- RULE: Covers1 (Atom)
covers' gamma pis (Primitive _ : as, q) = let pis' = expandAtom pis in covers gamma pis' (as, q)
-- RULE: Covers× (Tuple/Record)
covers' gamma pis (Tuple aMap : as, q) =
  let pis' = expandTuple pis in covers gamma pis' (elems aMap <> as, q)
covers' gamma pis (Record aMap : as, q) =
  let pis' = expandRecord pis in covers gamma pis' (elems aMap <> as, q)
-- RULE: Covers+ (Variant)
covers' gamma pis (Variant _ aMap : as, q) =
  let pisMap  = expandVariant pis
      missing = Map.traverseMissing $ \_ _ -> Nothing
      tuple   = Map.zipWithAMatched $ \_ branches a -> Just (branches, a)
  in  case Map.mergeA missing missing tuple pisMap aMap of
        Nothing      -> False
        Just pisAMap -> all (\(pisK, aK) -> covers gamma pisK (aK : as, q)) pisAMap
-- RULE: Covers∃
covers' gamma pis (Exists alpha k _ : as, q) =
  let gamma' = Ctx.add gamma $ DeclareUniversal alpha k in covers gamma' pis (as, q)
-- RULE: Covers∧
covers' gamma pis (With a0 prop : as, Principal) =
  coversAssuming gamma prop pis (a0 : as, Principal)
-- RULE: Covers∧!/ (Nonprincipal)
covers' gamma pis (With a0 _ : as, Nonprincipal) = covers gamma pis (a0 : as, Nonprincipal) -- FIXME: I think there's a typo regarding this rule in the paper
-- TODO: CoversVec
-- covers gamma pis (Vector t a : as, Principal) =
--   let (pisNil, pisCons) = expandVector pis
--   in  guarded pi && coversAssuming gamma (Equals t Zero) pisNil (as, Principal)
-- TODO: CoversVec!/ (Nonprincipal)
-- RULE: CoversVar
covers' gamma pis (_ : as, q) = let pis' = expandVariable pis in covers gamma pis' (as, q)
covers' _ _ _ = False

-- TODO
coversAssuming :: Context -> Proposition -> [Branch] -> ([Type], Principality) -> Bool
-- TODO: CoversEq
-- coversAssuming gamma (Equals t1 t2) pis (as, Principal) =
--   Equation.unify gamma (Ctx.apply gamma t1) (Ctx.apply gamma t2) -- FIXME: where to get kind k?
-- TODO: CoversEqBot
coversAssuming = undefined

guarded :: [Branch] -> Bool
guarded (Term.Branch { patterns } : pis) = case patterns of
  (Pattern.Vector _ []      : _) -> True
  (Pattern.Vector _ (_ : _) : _) -> True
  (Pattern.Wildcard _       : _) -> guarded pis
  (Pattern.Symbol _ _       : _) -> guarded pis
guarded _ = False

expandVector :: [Branch] -> ([Branch], [Branch])
expandVector [] = ([], [])
expandVector (Term.Branch { patterns = rho : rhos, body = e } : pis)
  | isSymbol rho || isWildcard rho
  = let
      (pisNil, pisCons) = expandVector pis
      left              = Term.Branch {patterns = rhos, body = e} : pisNil
      right =
        Term.Branch {patterns = Pattern.Wildcard () : Pattern.Wildcard () : rhos, body = e}
          : pisCons
    in
      (left, right)
expandVector (Term.Branch { patterns = Pattern.Vector _ [] : rhos, body = e } : pis) =
  let (pisNil, pisCons) = expandVector pis
  in  (Term.Branch {patterns = rhos, body = e} : pisNil, pisCons)
expandVector (Term.Branch { patterns = Pattern.Vector _ (rho : rho') : rhos, body = e } : pis) =
  let (pisNil, pisCons) = expandVector pis
  in  (pisNil, Term.Branch {patterns = rho : rho' <> rhos, body = e} : pisCons)
expandVector _ = error "[type.match] can only expand vector pattern"

expandTuple :: [Branch] -> [Branch]
expandTuple [] = []
expandTuple (Term.Branch { patterns = Pattern.Tuple _ rhoMap : rhos, body = e } : pis) =
  let pis' = expandTuple pis in Term.Branch {patterns = elems rhoMap <> rhos, body = e} : pis'
expandTuple (Term.Branch { patterns = rho : rhos, body = e } : pis)
  | isSymbol rho || isWildcard rho
  = let pis' = expandTuple pis
    in  (Term.Branch {patterns = Pattern.Wildcard () : Pattern.Wildcard () : rhos, body = e} : pis')
expandTuple _ = error "[type.match] can only expand tuple pattern"

expandRecord :: [Branch] -> [Branch]
expandRecord [] = []
expandRecord (Term.Branch { patterns = Pattern.Record _ rhoMap : rhos, body = e } : pis) =
  let pis' = expandRecord pis in Term.Branch {patterns = elems rhoMap <> rhos, body = e} : pis'
expandRecord (Term.Branch { patterns = rho : rhos, body = e } : pis)
  | isSymbol rho || isWildcard rho
  = let pis' = expandRecord pis
    in  (Term.Branch {patterns = Pattern.Wildcard () : Pattern.Wildcard () : rhos, body = e} : pis')
expandRecord _ = error "[type.match] can only expand record pattern"

expandVariant :: [Branch] -> Map Syntax.Keyword [Branch]
expandVariant [] = mempty
expandVariant (Term.Branch { patterns = Pattern.Variant _ k rho : rhos, body = e } : pis) =
  let pisKs      = expandVariant pis
      subpattern = Term.Branch {patterns = rho : rhos, body = e}
      prepend (Just pis) = Just $ subpattern : pis
      prepend Nothing    = Just [subpattern]
  in  Map.alter prepend k pisKs
expandVariant (Term.Branch { patterns = rho : rhos, body = e } : pis)
  | isSymbol rho || isWildcard rho
  = let pisKs      = expandVariant pis
        commonHead = Term.Branch {patterns = Pattern.Wildcard () : rhos, body = e}
    in  map (commonHead :) pisKs
expandVariant _ = error "[type.match] can only expand variant pattern"

expandVariable :: [Branch] -> [Branch]
expandVariable [] = []
expandVariable (Term.Branch { patterns = rho : rhos, body = e } : pis)
  | isSymbol rho || isWildcard rho
  = let pis' = expandVariable pis in Term.Branch {patterns = rhos, body = e} : pis'
expandVariable _ = error "[type.match] can only expand variable or wildcard pattern"

expandAtom :: [Branch] -> [Branch]
expandAtom [] = []
expandAtom (Term.Branch { patterns = rho : rhos, body = e } : pis)
  | isSymbol rho || isWildcard rho || isAtom rho
  = let pis' = expandAtom pis in Term.Branch {patterns = rhos, body = e} : pis'
expandAtom branches = error "[type.match] can only expand variable, wildcard or atom pattern"


isSymbol, isWildcard, isAtom :: Pattern -> Bool

isSymbol (Pattern.Symbol _ _) = True
isSymbol _                    = False

isWildcard (Pattern.Wildcard _) = True
isWildcard _                    = False

isAtom (Pattern.Atom _ _) = True
isAtom _                  = False
