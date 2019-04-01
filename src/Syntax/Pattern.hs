{-# LANGUAGE UndecidableInstances #-}

module Syntax.Pattern
  ( Pattern(..)
  )
where

import           Classes
import           Syntax.Atom                    ( Atom )
import           Syntax.Common
import           Syntax.Reference               ( Keyword
                                                , Value
                                                )


data Pattern phase
  = Wildcard (Context phase)
  | Symbol (Context phase)
           Value
  | Vector (Context phase)
           [Pattern phase]
  | Tuple (Context phase)
          (Map Int (Pattern phase))
  | Record (Context phase)
           Extent
           (Map Keyword (Pattern phase))
  | Variant (Context phase)
            Extent
            Keyword
            (Pattern phase)
  | Atom (Context phase)
         Atom
  deriving (Generic)

deriving instance (Show (Context phase)) => Show (Pattern phase)

deriving instance (Eq (Context phase)) => Eq (Pattern phase)

deriving instance (Ord (Context phase)) => Ord (Pattern phase)

instance Tree Pattern phase where
  walkM f = \case
    Vector ctx ps -> Vector ctx <$> traverse (walkM f) ps
    Tuple ctx pMap -> Tuple ctx <$> traverse (walkM f) pMap
    Record ctx extent pMap -> Record ctx extent <$> traverse (walkM f) pMap
    Variant ctx extent tag pat -> Variant ctx extent tag <$> walkM f pat
    p -> pure p
  metaM f =
    \case
      Wildcard ctx        -> Wildcard <$> f ctx
      Symbol ctx bind     -> Symbol <$> f ctx <*> pure bind
      Vector ctx ps       -> Vector <$> f ctx <*> traverse (metaM f) ps
      Tuple ctx pMap      -> Tuple <$> f ctx <*> traverse (metaM f) pMap
      Record ctx extent pMap     -> Record <$> f ctx <*> pure extent <*> traverse (metaM f) pMap
      Variant ctx extent tag pat -> Variant <$> f ctx <*> pure extent <*> pure tag <*> metaM f pat
      Atom ctx atom       -> Atom <$> f ctx <*> pure atom
