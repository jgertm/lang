module Type.Wellformedness where

import qualified Data.Map                      as Map
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Set                      as Set

import qualified Type.Context                  as Ctx
import           Type.Types

checkKind :: Context -> Type -> Kind -> Bool
-- RULE: VarSort
checkKind gamma (UniversalVariable uv) kind | (uv, kind) `Set.member` Ctx.universals gamma = True
checkKind gamma (ExistentialVariable ev) kind | (ev, kind) `Set.member` Ctx.existentials gamma =
  True
-- RULE: SolvedVarSort
checkKind gamma (ExistentialVariable ev) kind
  | (snd <$> Map.lookup ev (Ctx.existentialSolutions gamma)) == Just kind = True
-- RULE: UnitSort
checkKind _ (Primitive _) Type = True
-- RULE: BinSort
checkKind gamma (Function tau1 tau2) Type
  | checkKind gamma tau1 Type && checkKind gamma tau2 Type = True
checkKind gamma (Variant _ taus) Type    = all (\tau -> checkKind gamma tau Type) taus
checkKind gamma (Tuple  taus   ) Type    = all (\tau -> checkKind gamma tau Type) taus
checkKind gamma (Record taus   ) Type    = all (\tau -> checkKind gamma tau Type) taus
-- RULE: ZeroSort
checkKind _     Zero             Natural = True
-- RULE: SuccSort
checkKind gamma (Succ t)         Natural = checkKind gamma t Natural
checkKind _     _                _       = False

checkProposition :: Context -> Proposition -> Bool
-- RULE: EqProp
checkProposition gamma (Equals t t') = checkKind gamma t Natural && checkKind gamma t' Natural

checkType :: Context -> Type -> Bool
-- RULE: VarWF
checkType gamma (UniversalVariable uv) = uv `Set.member` (Set.map fst $ Ctx.universals gamma)
checkType gamma (ExistentialVariable ev)
  | ev `Set.member` (Set.map fst $ Ctx.existentials gamma) = True
  | Map.member ev (Ctx.existentialSolutions gamma) = True
  | otherwise = False
-- RULE: SolvedVarWF
-- RULE: UnitWF
checkType _     (Primitive _        ) = True
-- RULE: BinWF
checkType gamma (Function a b       ) = checkType gamma a && checkType gamma b
checkType gamma (Variant  _ as      ) = all (checkType gamma) as
checkType gamma (Tuple  as          ) = all (checkType gamma) as
checkType gamma (Record as          ) = all (checkType gamma) as
-- RULE: VecWF
checkType gamma (Vector t a         ) = checkKind gamma t Natural && checkType gamma a
-- RULE: ForallWF
checkType gamma (Forall alpha kind a) = checkType (Ctx.add gamma $ DeclareUniversal alpha kind) a
-- RULE: ExistsWF
checkType gamma (Exists alpha kind a) = checkType (Ctx.add gamma $ DeclareUniversal alpha kind) a
-- RULE: ImpliesWF
checkType gamma (Implies p a        ) = checkProposition gamma p && checkType gamma a
-- RULE: WithWF
checkType gamma (With    a p        ) = checkProposition gamma p && checkType gamma a
checkType _     Zero                  = True
checkType gamma (Succ t)              = checkKind gamma t Natural

checkTypeWithPrincipality :: Context -> (Type, Principality) -> Bool
-- RULE: PrincipalWF
checkTypeWithPrincipality gamma (a, Principal) =
  checkType gamma a && (Ctx.freeExistentialVariables gamma (Ctx.apply gamma a) == Set.empty)
-- RULE: NonPrincipalWF
checkTypeWithPrincipality gamma (a, Nonprincipal) = checkType gamma a

checkTypes :: Context -> [Type] -> Maybe Principality -> Bool
-- RULE: TypevecWF
checkTypes gamma ts Nothing  = all (checkType gamma) ts
-- RULE: PrincipalTypevecWF
checkTypes gamma ts (Just p) = all (\t -> checkTypeWithPrincipality gamma (t, p)) ts

checkContext :: Context -> Bool
-- RULE: EmptyCtx
checkContext (Context Empty) = True
-- RULE: HypCtx
checkContext (Context (rest :|> Binding x a Nonprincipal)) =
  let gamma = Context rest
  in  and
        [ checkContext gamma
        , checkType gamma a
        , (not $ any
            (\case
              Binding x' _ _ -> x == x'
              _              -> False
            )
            rest
          )
        ]
-- RULE: Hyp!Ctx
checkContext (Context (rest :|> Binding x a Principal)) =
  let gamma = Context rest
  in  and
        [ checkContext gamma
        , checkType gamma a
        , not $ any
          (\case
            Binding x' _ _ -> x == x'
            _              -> False
          )
          rest
        , Ctx.freeExistentialVariables gamma (Ctx.apply gamma a) == Set.empty
        ]
-- RULE: VarCtx
checkContext (Context (rest :|> DeclareUniversal uv _)) =
  let gamma = Context rest
  in  and
        [ checkContext gamma
        , (not $ any
            (\case
              DeclareUniversal uv' _ -> uv == uv'
            -- SolvedUniversal uv' _ | uv == uv' -> True
              _                      -> False
            )
            rest
          )
        ]
checkContext (Context (rest :|> DeclareExistential ev _)) =
  let gamma = Context rest
  in  and
        [ checkContext gamma
        , (not $ any
            (\case
              DeclareExistential ev' _ -> ev == ev'
              _                        -> False
            )
            rest
          )
        ]
-- RULE: SolvedCtx
checkContext (Context (rest :|> SolvedExistential ev kind t)) =
  let gamma = Context rest
  in  and
        [ checkContext gamma
        , (not $ any
            (\case
              SolvedExistential ev' _ _ -> ev == ev'
              _                         -> False
            )
            rest
          )
        , checkKind gamma t kind
        ]
-- RULE: EqnVarCtx
checkContext (Context (rest :|> SolvedUniversal uv tau)) =
  let gamma          = Context rest
      Just (_, kind) = find (\(uv', _) -> uv == uv') $ Ctx.universals gamma
  in  and
        [ checkContext gamma
        , not $ any
          (\case
            SolvedUniversal uv' _ -> uv == uv'
            _                     -> False
          )
          rest
        , checkKind gamma tau kind
        ]
-- RULE: MarkerCtx
checkContext (Context (rest :|> Marker u)) =
  let gamma = Context rest
  in  checkContext gamma && not
        (any
          (\case
            Marker u' -> u == u'
            _         -> False
          )
          rest
        )
