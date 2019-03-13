module Type.Equation
  ( incorporate
  , equals
  , true
  , unify
  )
where

import qualified Data.Set                      as Set

import           Error                          ( TypeError(..) )
import qualified Type.Context                  as Ctx
import           Type.Expression
import qualified Type.Instantiation            as Instantiate
import           Type.Monad
import           Type.Types

true :: Context -> Proposition -> Infer Context
-- RULE: CheckpropEq
true gamma (Equals t1 t2) = equals gamma t1 (t2, Natural)

incorporate :: Context -> Proposition -> Infer Context
-- RULE: ElimpropEq
incorporate gamma (Equals t1 t2) = unify gamma t1 (t2, Natural)

equals :: Context -> Type -> (Type, Kind) -> Infer Context
-- RULE: CheckeqVar
equals gamma u1 (u2, _) | u1 == u2 && isVariable u1            = pure gamma
-- RULE: CheckeqUnit
equals gamma (Primitive p1) (Primitive p2, Type) | p1 == p2    = pure gamma
-- RULE: CheckeqBin
equals gamma (Function tau1 tau2) (Function tau1' tau2', Type) = do
  theta <- equals gamma tau1 (tau1', Type)
  equals theta (Ctx.apply theta tau2) (Ctx.apply theta tau2', Type)
-- RULE: CheckeqZero
equals gamma Zero      (Zero   , Natural) = pure gamma
-- RULE: CheckeqSucc
equals gamma (Succ t1) (Succ t2, Natural) = equals gamma t1 (t2, Natural)
-- RULE: CheckeqInstL
equals gamma alpha@(ExistentialVariable ev) (t, kind)
  | (ev, kind)
    `Set.member`    Ctx.existentials gamma
    &&              ev
    `Set.notMember` Ctx.freeExistentialVariables gamma t
  = Instantiate.to gamma alpha (t, kind)
-- RULE: CheckeqInstR
equals gamma t (alpha@(ExistentialVariable ev), kind)
  | (ev, kind)
    `Set.member`    Ctx.existentials gamma
    &&              ev
    `Set.notMember` Ctx.freeExistentialVariables gamma t
  = Instantiate.to gamma t (alpha, kind)

clash :: Type -> Type -> Bool
clash Function{}       Function{}        = False
clash Tuple{}          Tuple{}           = False
clash Record{}         Record{}          = False
clash Variant{}        Variant{}         = False
clash Zero             Zero              = False
clash Succ{}           Succ{}            = False
clash (Primitive prim) (Primitive prim') = prim == prim'
clash _                _                 = True

unify :: Context -> Type -> (Type, Kind) -> Infer Context
-- RULE: ElimeqUvarRefl
unify gamma (UniversalVariable alpha) (UniversalVariable alpha', _) | alpha == alpha' = pure gamma
-- RULE: ElimeqZero
unify gamma Zero         (Zero    , Natural) = pure gamma
-- RULE: ElimeqSucc
unify gamma (Succ sigma) (Succ tau, Natural) = unify gamma sigma (tau, Natural)
-- RULE: ElimeqUvarL
unify gamma (UniversalVariable alpha) (tau, _)
  | alpha `Set.notMember` Ctx.freeUniversalVariables gamma tau = pure
  $ Ctx.add gamma (SolvedUniversal alpha tau)
-- RULE: ElimeqUvarR
unify gamma tau (UniversalVariable alpha, _)
  | alpha `Set.notMember` Ctx.freeUniversalVariables gamma tau = pure
  $ Ctx.add gamma (SolvedUniversal alpha tau)
-- RULE: ElimeqUnit
unify gamma (Primitive pt1) (Primitive pt2, Type) | pt1 == pt2 = pure gamma
-- RULE: ElimeqBin
unify gamma (Function tau1 tau2) (Function tau1' tau2', Type)  = do
  theta <- unify gamma tau1 (tau1', Type)
  unify theta (Ctx.apply theta tau2) (Ctx.apply theta tau2', Type)
-- RULE: ElimeqClash
unify _ sigma (tau, _) | clash sigma tau = throwError UnificationError
-- RULE: ElimeqUvarLBot
-- RULE: ElimeqUvarRBot
-- RULE: ElimeqBinBot
unify _ _ _                              = throwError UnificationError
