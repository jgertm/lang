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
  deriving (Generic)

deriving instance (Show (Context phase), Show (Extra phase)) => Show (Definition phase)

deriving instance (Eq (Context phase), Eq (Extra phase)) => Eq (Definition phase)

deriving instance (Ord (Context phase), Ord (Extra phase)) => Ord (Definition phase)

instance Tree Definition phase where
  walkM f =
          \case
            Module ctx name defs ->
              Module ctx name <$> traverse (walkM f) defs
            Type ctx name typ -> pure $ Type ctx name typ
            Constant ctx name body -> pure $ Constant ctx name body
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
