{-# LANGUAGE DataKinds #-}

module Type.Expression where

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
          Function type1 type2 -> Function (subst type1) (subst type2)
          Variant types        -> Variant (map subst types)
          Tuple   types        -> Tuple (map subst types)
          Record  types        -> Record (map subst types)
          Forall tv kind typ   -> Forall tv kind (subst typ)
          Exists tv kind typ   -> Exists tv kind (subst typ)
          Implies prop typ     -> Implies prop (subst typ)
          With    typ  prop    -> With (subst typ) prop
          Succ typ             -> Succ (subst typ)
          Vector type1 type2   -> Vector (subst type1) (subst type2)
          _                    -> expr
