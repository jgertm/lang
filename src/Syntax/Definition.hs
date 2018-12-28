{-# LANGUAGE UndecidableInstances #-}

module Syntax.Definition where

import           Classes
import           Syntax.Common                  ( Binding )
import           Syntax.Term                    ( Term )
import           Syntax.Type                    ( Type )

data Definition phase
  = Module (Context phase)
           Binding
           [Definition phase]
  | Type (Context phase)
         Binding
         (Type phase)
  | Constant (Context phase)
             Binding
             (Term phase)
  | Function (Context phase)
             Binding
             [Binding] -- ^ arguments
             (Term phase) -- ^ body
  deriving (Generic)

deriving instance (Show (Context phase)) => Show (Definition phase)

deriving instance (Eq (Context phase)) => Eq (Definition phase)

deriving instance (Ord (Context phase)) => Ord (Definition phase)

instance Tree Definition phase where
  walkM dir f =
    let step =
          \case
            Module ctx name defs ->
              Module ctx name <$> traverse (walkM dir f) defs
            Type ctx name typ -> pure $ Type ctx name typ
            Constant ctx name body -> pure $ Constant ctx name body
            Function ctx name args body -> pure $ Function ctx name args body
     in case dir of
          Up   -> f <=< step
          Down -> step <=< f
  metaM f def =
    case def of
      Module ctx name defs -> do
        ctx' <- f ctx
        defs' <- traverse (metaM f) defs
        pure $ Module ctx' name defs'
      Type ctx name typ -> do
        ctx' <- f ctx
        typ' <- metaM f typ
        pure $ Type ctx' name typ'
      Constant ctx name term -> do
        ctx' <- f ctx
        term' <- metaM f term
        pure $ Constant ctx' name term'
      Function ctx name args body -> do
        ctx' <- f ctx
        body' <- metaM f body
        pure $ Function ctx' name args body'
