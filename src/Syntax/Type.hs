{-# LANGUAGE UndecidableInstances #-}

module Syntax.Type
  ( Type(..)
  )
where

import           Classes
import qualified Syntax.Reference              as Ref

data Type phase
  = Named (Context phase) Ref.Type
  | Application (Context phase)
                Ref.Type
                [Type phase]
  | Tuple (Context phase)
          [Type phase]
  | Record (Context phase)
           [(Ref.Keyword, Type phase)]
  | Variant (Context phase)
            [(Ref.Keyword, Type phase)]
  | Function (Context phase)
             [Type phase]
  deriving (Generic)

deriving instance (Show (Context phase)) => Show (Type phase)

deriving instance (Eq (Context phase)) => Eq (Type phase)

deriving instance (Ord (Context phase)) => Ord (Type phase)

instance Tree Type phase where
  walkM f =
    \case
      Named ctx name -> pure $ Named ctx name
      Application ctx operator params -> Application ctx operator <$> traverse down params
      Tuple ctx fields -> Tuple ctx <$> traverse down fields
      Record ctx fields -> Record ctx <$> (for fields $ \(name, typ) -> sequenceA (name, f typ))
      Variant ctx cases -> Variant ctx <$> (for cases $ \(tag, typ) -> sequenceA (tag, f typ))
      Function ctx types -> Function ctx <$> traverse down types
    where
      down = walkM f
  metaM f =
    \case
      Named ctx name -> do
        ctx' <- f ctx
        pure $ Named ctx' name
      Application ctx operator params -> do
        ctx' <- f ctx
        params' <- traverse (metaM f) params
        pure $ Application ctx' operator params'
      Tuple ctx fields -> do
        ctx' <- f ctx
        fields' <- traverse (metaM f) fields
        pure $ Tuple ctx' fields'
      Record ctx fields -> do
        ctx' <- f ctx
        fields' <- for fields $ \(name, typ) -> sequenceA (name, metaM f typ)
        pure $ Record ctx' fields'
      Variant ctx cases -> do
        ctx' <- f ctx
        cases' <- for cases $ \(tag, typ) -> sequenceA (tag, metaM f typ)
        pure $ Variant ctx' cases'
      Function ctx types -> do
        ctx' <- f ctx
        types' <- traverse (metaM f) types
        pure $ Function ctx' types'
