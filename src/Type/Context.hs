{-# LANGUAGE DataKinds #-}

module Type.Context
  ( empty, initialize
  , universals, existentials, existentialSolutions, definitions
  , freeUniversalVariables, freeExistentialVariables, existentialVariables
  , apply
  , add, adds, drop, split, inject, splice, join
  , solve, bind, lookup
  , substitute
  )
where

import qualified Data.Map.Strict               as Map
import           Data.Sequence                  ( Seq(..)
                                                , (|>)
                                                )
import qualified Data.Sequence                 as Seq
import qualified Data.Set                      as Set

import qualified Syntax.Reference              as Ref
import qualified Type.Expression               as Type
import           Type.Types


-- interface

empty :: Context
empty = Context mempty

initialize :: Map Ref.Type Type -> Map Ref.Value Type -> Context
initialize initialTypedefs initialBindings =
  let typedefs =
        concatMap
            (\(n, t) -> mconcat
              [ map (\evar -> DeclareExistential evar Type) $ Set.toList $ existentialVariables t
              , map (\uvar -> DeclareUniversal uvar Type) $ Set.toList $ universalVariables t
              , [Definition n Type t]
              ])
          $ Map.toList initialTypedefs
      bindings = map (\(n, t) -> Binding n t Principal) $ Map.toList initialBindings
  in Context $ Seq.fromList $ typedefs <> bindings

existentials :: Context -> Map (Variable 'Existential, Kind) (Maybe Type)
existentials gamma =
  let declarations = [ ((tv, kind), Nothing) | DeclareExistential tv kind <- toList $ unContext gamma ]
      solutions    = [ ((tv, kind), Just solution) | SolvedExistential tv kind solution <- toList $ unContext gamma ]
  in  (Map.union `on` Map.fromList) declarations solutions

universals :: Context -> Set (Variable 'Universal, Kind)
universals gamma =
  let declarations = [ (tv, kind) | DeclareUniversal tv kind <- toList $ unContext gamma ]
      solutions    = [ (tv, undefined) | SolvedUniversal tv _ <- toList $ unContext gamma ] -- FIXME
  in  (Set.union `on` Set.fromList) declarations solutions

freeExistentialVariables :: Context -> Type -> Set (Variable 'Existential)
freeExistentialVariables ctx typ = case typ of
  Primitive _                 -> Set.empty
  Function type1 type2        -> freeExistentialVariables ctx type1 <> freeExistentialVariables ctx type2
  Variant  (Open alpha) types -> Set.delete alpha $ foldMap (freeExistentialVariables ctx) types
  Variant  _            types -> foldMap (freeExistentialVariables ctx) types
  Tuple types                 -> foldMap (freeExistentialVariables ctx) types
  Record (Open alpha) types   -> Set.delete alpha $ foldMap (freeExistentialVariables ctx) types
  Record _            types   -> foldMap (freeExistentialVariables ctx) types
  UniversalVariable _         -> Set.empty
  ExistentialVariable ev ->
    if Map.notMember ev (existentialSolutions ctx) then Set.singleton ev else Set.empty
  Exists _ _ body    -> freeExistentialVariables ctx body
  Forall _ _ body    -> freeExistentialVariables ctx body
  Fix     exvar body -> Set.delete exvar $ freeExistentialVariables ctx body
  Implies _     body -> freeExistentialVariables ctx body
  With    body  _    -> freeExistentialVariables ctx body

freeUniversalVariables :: Context -> Type -> Set (Variable 'Universal)
freeUniversalVariables ctx typ = case typ of
  UniversalVariable uv ->
    if Map.notMember uv (universalSolutions ctx) then Set.singleton uv else Set.empty
  Function type1 type2 -> freeUniversalVariables ctx type1 <> freeUniversalVariables ctx type2
  Variant  _     types -> foldMap (freeUniversalVariables ctx) types
  Tuple types          -> foldMap (freeUniversalVariables ctx) types
  Record _ types       -> foldMap (freeUniversalVariables ctx) types
  Exists _ _ body      -> freeUniversalVariables ctx body
  Forall _ _ body      -> freeUniversalVariables ctx body

apply :: Context -> Type -> Type
apply ctx typ = case typ of
  Primitive _           -> typ
  Function type1  type2 -> Function (apply ctx type1) (apply ctx type2)
  Variant  rowvar types -> Variant rowvar (map (apply ctx) types)
  Tuple types           -> Tuple (map (apply ctx) types)
  Record rowvar types   -> Record rowvar (map (apply ctx) types)
  UniversalVariable var -> case Map.lookup var $ universalSolutions ctx of
    Just tau -> apply ctx tau
    Nothing  -> typ
  ExistentialVariable var -> case Map.lookup var $ existentialSolutions ctx of
    Just (tau, _) -> apply ctx tau
    Nothing       -> typ
  Forall var kind t   -> Forall var kind (apply ctx t)
  Exists var kind sub -> Exists var kind (apply ctx sub)
  Implies prop sub    -> Implies prop (apply ctx sub)
  With    sub  prop   -> With (apply ctx sub) prop
  Zero                -> Zero
  Succ sub            -> Succ (apply ctx sub)
  Vector type1 type2  -> Vector (apply ctx type1) (apply ctx type2)
  Fix    var   sub    -> Fix var (apply ctx sub)

add :: Context -> Fact -> Context
add ctx elt = adds ctx [elt]

adds :: Context -> [Fact] -> Context
adds (Context facts) els = Context $ facts <> (Seq.fromList els)

split :: Context -> Fact -> (Context, Context)
split (Context ctx) fact = case Seq.breakr (fact ==) ctx of
  (post, pre :|> _) -> (Context pre, Context post)
  _                 -> error "Failed to split context"

inject :: Context -> Fact -> Context -> Context
inject pre elt post = splice pre [elt] post

join :: Context -> Context -> Context
join pre post = splice pre [] post

drop :: Context -> Fact -> Context
drop ctx elt = let (pre, _) = split ctx elt in pre

splice :: (Foldable t) => Context -> t Fact -> Context -> Context
splice (Context pre) els (Context post) = Context $ mconcat [pre, Seq.fromList $ toList els, post]

solve :: Context -> (Variable 'Existential, Kind) -> Type -> Context
solve context (exvar, kind) typ =
  let (pre, post) = split context (DeclareExistential exvar kind)
      solution = SolvedExistential exvar kind typ
  in inject pre solution post

bind :: Context -> Ref.Value -> Type -> Principality -> Context
bind context ref typ p =
  add context $ Binding ref typ p

lookup :: Context -> Ref.Value -> Maybe (Type, Principality)
lookup (Context facts) ref =
  let pred (Binding ref' _ _) = ref == ref'
      pred _ = False
  in case (Seq.filter pred facts) of
       _ :|> Binding _ typ p -> Just (typ, p)
       Empty -> Nothing

-- implementation

existentialSolutions :: Context -> Map (Variable 'Existential) (Type, Kind)
existentialSolutions gamma =
  Map.fromList [ (tv, (typ, kind)) | SolvedExistential tv kind typ <- toList $ unContext gamma ]

universalSolutions :: Context -> Map (Variable 'Universal) Type
universalSolutions gamma =
  Map.fromList [ (tv, typ) | SolvedUniversal tv typ <- toList $ unContext gamma ]

bindings :: Context -> Map Ref.Value (Type, Principality)
bindings gamma = Map.fromList
  [ (binding, (typ, principality)) | Binding binding typ principality <- toList $ unContext gamma ]

definitions :: Context -> Map Ref.Type (Type, Kind)
definitions gamma =
  Map.fromList [ (name, (kind, typ)) | Definition name typ kind <- toList $ unContext gamma ]

substitute :: Context -> Type -> Type -> Context
substitute (Context ctx) match replacement = Context $ map replace ctx
 where
  replace :: Fact -> Fact
  replace = \case
    Binding name typ principality ->
      Binding name (Type.substitute typ match replacement) principality
    SolvedUniversal var typ -> SolvedUniversal var (Type.substitute typ match replacement)
    SolvedExistential var kind typ ->
      SolvedExistential var kind (Type.substitute typ match replacement)
    element -> element

existentialVariables :: Type -> Set (Variable 'Existential)
existentialVariables typ = case typ of
  Primitive _                 -> Set.empty
  Function type1        type2 -> existentialVariables type1 <> existentialVariables type2
  Variant  (Open alpha) types -> Set.delete alpha $ foldMap existentialVariables types
  Variant  _            types -> foldMap existentialVariables types
  Tuple types                 -> foldMap existentialVariables types
  Record (Open alpha) types   -> Set.delete alpha $ foldMap existentialVariables types
  Record _            types   -> foldMap existentialVariables types
  UniversalVariable   _       -> Set.empty
  ExistentialVariable ev      -> Set.singleton ev
  Exists _ _ body             -> existentialVariables body
  Forall _ _ body             -> existentialVariables body
  Fix     exvar body          -> existentialVariables body
  Implies _     body          -> existentialVariables body
  With    body  _             -> existentialVariables body

universalVariables :: Type -> Set (Variable 'Universal)
universalVariables typ = case typ of
  Primitive _                 -> Set.empty
  Function type1        type2 -> universalVariables type1 <> universalVariables type2
  Variant  _            types -> foldMap universalVariables types
  Tuple types                 -> foldMap universalVariables types
  Record _            types   -> foldMap universalVariables types
  UniversalVariable   uv      -> Set.singleton uv
  ExistentialVariable ev      -> Set.empty
  Exists _ _ body             -> universalVariables body
  Forall _ _ body             -> universalVariables body
  Fix     exvar body          -> universalVariables body
  Implies _     body          -> universalVariables body
  With    body  _             -> universalVariables body
