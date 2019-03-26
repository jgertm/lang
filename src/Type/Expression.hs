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
  , findVariant
  , findRecord
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

fromSyntax :: Map Ref.Type Type -> Syntax.Type phase -> Type
fromSyntax typedefs (Syntax.Named _ typ@(Ref.Type name)) =
  fromMaybe (error $ "[type.expression] unknown named type: " <> name) $ Map.lookup typ typedefs
fromSyntax typedefs (Syntax.Tuple _ fields) =
  Tuple (map (fromSyntax typedefs) $ Map.fromList $ zip [1 ..] fields)
fromSyntax typedefs (Syntax.Record _ fields) =
  Record Closed (map (fromSyntax typedefs) $ Map.fromList fields)
fromSyntax typedefs (Syntax.Variant _ cases) =
  Variant Closed (map (fromSyntax typedefs) $ Map.fromList cases)
fromSyntax typedefs (Syntax.Function _ types) = fn $ map (fromSyntax typedefs) types

fromDefinition :: Def.Definition phase -> Infer (Type, Context)
fromDefinition (Def.Type _ name params body) = do
  typedefs  <- ask
  nameVar   <- freshExistential
  paramVars <- traverse (const freshExistential) params
  let paramTypes  = Map.fromList $ zip params $ map ExistentialVariable paramVars
      newTypedefs = Map.insert name (ExistentialVariable nameVar) $ Map.union paramTypes typedefs
      typ         = Fix nameVar $ fromSyntax (Map.union newTypedefs typedefs) body
      ctx         = Context $ Seq.fromList $ map (`DeclareExistential` Type) (nameVar : paramVars)
  pure (typ, ctx)
fromDefinition _ =
  error "[type.expression] only type definitions can be converted to type expressions"

findVariant :: Ref.Keyword -> Map Ref.Type Type -> Maybe Type
findVariant tag =
  let match (Variant Closed tags) = tag `Map.member` tags
      match (Fix     _      body) = match body
      match _                     = False
  in  find match

findRecord :: Set Ref.Keyword -> Map Ref.Type Type -> Maybe Type
findRecord fields =
  let match (Record Closed fieldMap) = fields == Map.keysSet fieldMap
      match (Fix    _      body    ) = match body
      match _                        = False
  in  find match
