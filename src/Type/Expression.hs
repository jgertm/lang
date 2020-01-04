{-# LANGUAGE DataKinds #-}

module Type.Expression
  ( substitute
  , contains
  , fn
  , unit
  , integer
  , rational
  , real
  , string
  , boolean
  , natives
  , isVariable
  , isQuantified
  , isUniversallyQuantified
  , isExistentiallyQuantified
  , isWith
  , fromSyntax
  , findVariant
  , findRecord
  )
where

import qualified Data.Map.Strict               as Map

import qualified Syntax.Reference              as Ref
import qualified Syntax.Type                   as Syntax
import           Type.Types


isVariable, isUniversallyQuantified, isExistentiallyQuantified, isQuantified, isWith :: Type -> Bool

isVariable UniversalVariable{}   = True
isVariable ExistentialVariable{} = True
isVariable _                     = False

isUniversallyQuantified Forall{} = True
isUniversallyQuantified _        = False

isExistentiallyQuantified Exists{} = True
isExistentiallyQuantified _        = False

isQuantified = liftA2 (||) isUniversallyQuantified isExistentiallyQuantified

isWith With{} = True
isWith _      = False

unit, integer, rational, real, string, boolean :: Type

unit = Primitive "Unit"
integer = Primitive "Integer"
rational = Primitive "Rational"
real = Primitive "Real"
string = Primitive "String"
boolean = Primitive "Boolean"

natives :: Map Ref.Type Type
natives = Map.fromList $ map
  (\case
    typ@(Primitive name) -> (Ref.Type name, typ)
    _                    -> error "[type.expression/natives] unnamed type in natives"
  )
  [unit, integer, rational, real, string, boolean]

fn :: [Type] -> Type
fn [] = error "[type.expression/fn] cannot construct function type without types"
fn ts = foldr1 Function ts

contains :: Type -> Type -> Bool
contains expr match
  | expr == match
  = True
  | otherwise
  = let cont typ = contains typ match
    in  case expr of
          Function type1 type2 -> cont type1 || cont type2
          Variant  _     types -> any cont types
          Tuple types          -> any cont types
          Record _ types       -> any cont types
          Forall _ _ typ       -> cont typ
          Exists _ _ typ       -> cont typ
          Implies _   typ      -> cont typ
          With    typ _        -> cont typ
          Succ typ             -> cont typ
          Vector type1 type2   -> cont type1 || cont type2
          _                    -> False

substitute :: Type -> Type -> Type -> Type
substitute expr match replacement
  | expr == match
  = replacement
  | otherwise
  = let subst typ = substitute typ match replacement
    in  case expr of
          Function type1  type2 -> Function (subst type1) (subst type2)
          Variant  rowvar types -> Variant rowvar (map subst types)
          Tuple types           -> Tuple (map subst types)
          Record rowvar types   -> Record rowvar (map subst types)
          Forall tv kind typ    -> Forall tv kind (subst typ)
          Exists tv kind typ    -> Exists tv kind (subst typ)
          Implies prop typ      -> Implies prop (subst typ)
          With    typ  prop     -> With (subst typ) prop
          Succ typ              -> Succ (subst typ)
          Vector type1 type2    -> Vector (subst type1) (subst type2)
          _                     -> expr

fromSyntax :: Map Ref.Type Type -> Syntax.Type phase -> Type
fromSyntax typedefs (Syntax.Named _ typ@(Ref.Type name)) =
  fromMaybe (error $ "[type.expression/from-syntax] unknown named type: " <> name)
    $ Map.lookup typ typedefs
fromSyntax typedefs (Syntax.Application _ typ@(Ref.Type name) params) =
  let typ' = fromMaybe (error $ "[type.expression/from-syntax] unknown named type in application: " <> name) $ Map.lookup typ typedefs
      params' = map (fromSyntax typedefs) params
  in Application typ' params'
fromSyntax typedefs (Syntax.Tuple _ fields) =
  Tuple (map (fromSyntax typedefs) $ Map.fromList $ zip [1 ..] fields)
fromSyntax typedefs (Syntax.Record _ fields) =
  Record Closed (map (fromSyntax typedefs) $ Map.fromList fields)
fromSyntax typedefs (Syntax.Variant _ cases) =
  Variant Closed (map (fromSyntax typedefs) $ Map.fromList cases)
fromSyntax typedefs (Syntax.Function _ types) = fn $ map (fromSyntax typedefs) types

findVariant :: Ref.Keyword -> Map Ref.Type Type -> Maybe (Ref.Type, Type)
findVariant tag =
  let match (Variant Closed tags) = tag `Map.member` tags
      match (Fix     _      body) = match body
      match (Abstraction _ body)  = match body
      match (Forall _ _ body)     = match body
      match _                     = False
  in  find (match . snd) . toPairs

findRecord :: Set Ref.Keyword -> Map Ref.Type Type -> Maybe (Ref.Type, Type)
findRecord fields =
  let match (Record Closed fieldMap) = fields == Map.keysSet fieldMap
      match (Fix    _      body    ) = match body
      match _                        = False
  in  find (match . snd) . toPairs
