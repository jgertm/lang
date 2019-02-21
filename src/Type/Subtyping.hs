{-|
This module implements the __SUBTYPING__ (@<:@) rules from /figure 22: Algorithmic subtyping and equivalence/ (page 42)
-}
module Type.Subtyping where

import qualified Data.Set                      as Set

import           Error                          ( TypeError(..) )
import qualified Type.Context                  as Ctx
import           Type.Equation                  ( equals )
import           Type.Expression
import qualified Type.Instantiation            as Instantiate
import           Type.Monad
import           Type.Rules
import           Type.Types

subtype, subtype' :: Context -> Polarity -> Type -> Type -> Infer Context

subtype gamma pol a b = subtype' gamma pol a b

-- RULE: <:Equiv
subtype' gamma _ a b | not (isQuantified a) && not (isQuantified b) = equivalent gamma a b
-- RULE: <:∀L
subtype' gamma Negative (Forall alpha kind a) b | not $ isUniversallyQuantified b = do
  alphaEx <- freshExistential
  let marker = Marker alphaEx
      gamma' = Ctx.adds gamma [marker, DeclareExistential alphaEx kind]
  ctx <- subtype gamma'
                 Negative
                 (substitute a (UniversalVariable alpha) (ExistentialVariable alphaEx))
                 b
  pure $ Ctx.drop ctx marker
-- RULE: <:∀R
subtype' gamma Negative a (Forall beta kind b) = do
  let declaration = DeclareUniversal beta kind
      gamma'      = Ctx.add gamma declaration
  ctx <- subtype gamma' Negative a b
  pure $ Ctx.drop ctx declaration
-- RULE: <:∃L
subtype' gamma Positive (Exists alpha kind a) b = do
  let declaration = DeclareUniversal alpha kind
      gamma'      = Ctx.add gamma declaration
  ctx <- subtype gamma' Positive a b
  pure $ Ctx.drop ctx declaration
-- RULE: <:∃R
subtype' gamma Positive a (Exists beta kind b) | not $ isExistentiallyQuantified a = do
  betaEx <- freshExistential
  let marker = Marker betaEx
      gamma' = Ctx.adds gamma [marker, DeclareExistential betaEx kind]
  ctx <- subtype gamma'
                 Positive
                 a
                 (substitute b (UniversalVariable beta) (ExistentialVariable betaEx))
  pure $ Ctx.drop ctx marker
subtype' gamma Positive a b |
  -- RULE: <:-+L
                              negative a && not (positive b) = subtype gamma Negative a b
                            |
  -- RULE: <:-+R
                              not (positive a) && negative b = subtype gamma Negative a b
subtype' gamma Negative a b |
  -- RULE: <:+-L
                              positive a && not (negative b) = subtype gamma Positive a b
                            |
  -- RULE: <:+-R
                              not (negative a) && positive b = subtype gamma Positive a b
subtype' _ _ _ _ = throwError SubtypingError

equivalent, equivalent' :: Context -> Type -> Type -> Infer Context

equivalent gamma a b = equivalent' gamma a b

-- RULE: ≡Var
equivalent' gamma alpha1@(UniversalVariable _) alpha2@(UniversalVariable _) | alpha1 == alpha2 =
  pure gamma
-- RULE: ≡Exvar
equivalent' gamma alpha1@(ExistentialVariable _) alpha2@(ExistentialVariable _) | alpha1 == alpha2 =
  pure gamma
-- RULE: ≡Unit
equivalent' gamma prim@(Primitive _) prim'@(Primitive _) | prim == prim' = pure gamma
-- RULE: ≡⊕
equivalent' gamma (Function a1 a2) (Function b1 b2)                      = do
  theta <- equivalent gamma a1 b1
  equivalent theta (Ctx.apply theta a2) (Ctx.apply theta b2)
-- TODO: ≡⊕ for variants, tuples and records
-- RULE: ≡Vec
equivalent' gamma (Vector t1 a1) (Vector t2 a2) = do
  theta <- equivalent gamma t1 t2
  equivalent theta (Ctx.apply theta a1) (Ctx.apply theta a2)
-- RULE: ≡∀
equivalent' gamma (Forall alpha1 kind1 a) (Forall alpha2 kind2 b)
  | alpha1 == alpha2 && kind1 == kind2 = do
    let declaration = DeclareUniversal alpha1 kind1
    ctx <- equivalent (Ctx.add gamma declaration) a b
    pure $ Ctx.drop ctx declaration
-- RULE: ≡∃
equivalent' gamma (Exists alpha1 kind1 a) (Exists alpha2 kind2 b)
  | alpha1 == alpha2 && kind1 == kind2 = do
    let declaration = DeclareUniversal alpha1 kind1
    ctx <- equivalent (Ctx.add gamma declaration) a b
    pure $ Ctx.drop ctx declaration
-- RULE: ≡⊃
equivalent' gamma (Implies p a) (Implies q b) = do
  theta <- propositionsEquivalent gamma p q
  equivalent theta (Ctx.apply theta a) (Ctx.apply theta b)
-- RULE: ≡∧
equivalent' gamma (With a p) (With b q) = do
  theta <- propositionsEquivalent gamma p q
  equivalent theta (Ctx.apply theta a) (Ctx.apply theta b)
-- RULE: ≡InstantiateL
equivalent' gamma alpha@(ExistentialVariable ev) tau
  | (ev, Type)
    `Set.member`    Ctx.existentials gamma
    &&              ev
    `Set.notMember` Ctx.freeExistentialVariables gamma tau
  = Instantiate.to gamma alpha (tau, Type)
-- RULE: ≡InstantiateR
equivalent' gamma tau alpha@(ExistentialVariable ev)
  | (ev, Type)
    `Set.member`    Ctx.existentials gamma
    &&              ev
    `Set.notMember` Ctx.freeExistentialVariables gamma tau
  = Instantiate.to gamma alpha (tau, Type)
equivalent' gamma a b = throwError SubtypingError

propositionsEquivalent :: Context -> Proposition -> Proposition -> Infer Context
propositionsEquivalent gamma (Equals t1 t1') (Equals t2 t2') = do
  theta <- equals gamma t1 (t2, Natural)
  equals theta (Ctx.apply theta t1') (Ctx.apply theta t2', Natural)

polarity :: Type -> Polarity
polarity Forall{} = Negative
polarity Exists{} = Positive
polarity _        = Neutral

positive, negative :: Type -> Bool
positive t = polarity t == Positive

negative t = polarity t == Negative

join :: Polarity -> Polarity -> Polarity
join Positive _       = Positive
join Negative _       = Negative
join Neutral  Neutral = Negative
join Neutral  pol     = pol
