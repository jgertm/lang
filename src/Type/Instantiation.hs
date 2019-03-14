module Type.Instantiation
  ( to
  )
where

import qualified Data.Set                      as Set

import           Error                          ( TypeError(..) )
import qualified Type.Context                  as Ctx
import           Type.Monad
import           Type.Types
import qualified Type.Wellformedness           as Wellformed


to, to' :: Context -> Type -> (Type, Kind) -> Infer Context

to gamma a tk = to' gamma a tk

-- RULE: InstReach
to' gamma alphaTyp@(ExistentialVariable alpha) (ExistentialVariable beta, k)
  | Ctx.isUnsolved beta gamma && Ctx.before gamma alpha beta = do
    let (pre, post) = Ctx.split gamma (DeclareExistential beta k)
    pure $ Ctx.inject pre (SolvedExistential beta k alphaTyp) post
-- RULE: InstBin
to' gamma (ExistentialVariable alpha) (Function tau1 tau2, Type)
  | (alpha, Type) `Set.member` Ctx.existentials gamma = do
    alpha1 <- freshExistential
    alpha2 <- freshExistential
    let alphaType1  = ExistentialVariable alpha1
        alphaType2  = ExistentialVariable alpha2
        function    = Function alphaType1 alphaType2
        (pre, post) = Ctx.split gamma (DeclareExistential alpha Type)
        gamma'      = Ctx.splice
          pre
          [ DeclareExistential alpha2 Type
          , DeclareExistential alpha1 Type
          , SolvedExistential alpha Type function
          ]
          post
    theta <- to gamma' alphaType1 (tau1, Type)
    to theta alphaType2 (Ctx.apply theta tau2, Type)
-- TODO: InstBin for variants, tuples and records
-- RULE: InstZero
to' gamma (ExistentialVariable alpha) (Zero, Natural) = do
  let (pre, post) = Ctx.split gamma (DeclareExistential alpha Natural)
  pure $ Ctx.inject pre (SolvedExistential alpha Natural Zero) post
-- RULE: InstSucc
to' gamma (ExistentialVariable alpha) (Succ t1, Natural) = do
  alpha1 <- freshExistential
  let alphaType1  = ExistentialVariable alpha1
      (pre, post) = Ctx.split gamma (DeclareExistential alpha Natural)
      gamma'      = Ctx.splice
        pre
        [DeclareExistential alpha1 Natural, SolvedExistential alpha Natural (Succ alphaType1)]
        post
  to gamma' alphaType1 (t1, Natural)
-- RULE: InstSolve
to' gamma (ExistentialVariable alpha) (tau, k) = do
  let (gamma0, gamma1) = Ctx.split gamma (DeclareExistential alpha k)
  unless (Wellformed.checkKind gamma0 tau k) $ typeerror (InstantiationError "C")
  pure $ Ctx.inject gamma0 (SolvedExistential alpha k tau) gamma1
to' _ _ _ = typeerror (RuleError "InstFallthrough")
