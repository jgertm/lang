{-# LANGUAGE DataKinds #-}

module Type.Expression
  ( substitute
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
  , fromDefinition
  )
where

import qualified Data.Map.Strict               as Map
import qualified Data.Sequence                 as Seq

import qualified Syntax.Definition             as Def
import qualified Syntax.Reference              as Ref
import qualified Syntax.Type                   as Syntax
import           Type.Monad
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
    _                    -> error "[type.expression] unnamed type in natives"
  )
  [unit, integer, rational, real, string, boolean]

fn :: [Type] -> Type
fn []       = error "Cannot construct function type without types"
fn (t : ts) = foldr Function t ts

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

fromSyntaxWith :: Map Ref.Type Type -> Syntax.Type phase -> Type
fromSyntaxWith bindings (Syntax.Named _ typ@(Ref.Type name)) =
  fromMaybe (error $ "[type.expression] unknown named type: " <> name) $ Map.lookup typ bindings
fromSyntaxWith bindings (Syntax.Tuple _ fields) =
  Tuple (map (fromSyntaxWith bindings) $ Map.fromList $ zip [1 ..] fields)
fromSyntaxWith bindings (Syntax.Record _ fields) =
  Record Closed (map (fromSyntaxWith bindings) $ Map.fromList fields)
fromSyntaxWith bindings (Syntax.Variant _ cases) =
  Variant Closed (map (fromSyntaxWith bindings) $ Map.fromList cases)
fromSyntaxWith bindings (Syntax.Function _ types) = fn $ map (fromSyntaxWith bindings) types

fromDefinition :: Map Ref.Type Type -> Def.Definition phase -> Infer (Type, Context)
fromDefinition bindings (Def.Type _ name params body) = do
  nameVar   <- freshExistential
  paramVars <- traverse (const freshExistential) params
  let paramTypes  = Map.fromList $ zip params $ map ExistentialVariable paramVars
      newBindings = Map.insert name (ExistentialVariable nameVar) $ Map.union paramTypes bindings
      typ         = Fix nameVar $ fromSyntaxWith (Map.union newBindings bindings) body
      ctx         = Context $ Seq.fromList $ map (`DeclareExistential` Type) (nameVar : paramVars)
  pure (typ, ctx)
fromDefinition _ _ =
  error "[type.expression] only type definitions can be converted to type expressions"
