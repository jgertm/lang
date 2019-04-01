{-# LANGUAGE UndecidableInstances #-}

module Syntax.Term
  ( Term(..)
  , Branch(..)
  )
where

import           Data.Bitraversable

import           Classes
import           Syntax.Atom                    ( Atom )
import           Syntax.Common
import           Syntax.Pattern                 ( Pattern )
import           Syntax.Reference               ( Keyword
                                                , Value
                                                )
import           Syntax.Type                    ( Type )


data Term phase
  = Lambda (Context phase)
           Value -- ^ argument name
           (Term phase) -- ^ body
  | If (Context phase)
       (Term phase) -- ^ test
       (Term phase) -- ^ true
       (Term phase) -- ^ false
  | Match (Context phase)
          (Term phase) -- ^ prototype
          [Branch phase] -- ^ match clauses
  | Sequence (Context phase)
             [Term phase]
  | Let (Context phase)
        [( Value -- ^ bound name
         , Term phase -- ^ body
          )]
        (Term phase) -- ^ scope
  | Application (Context phase)
                (Term phase) -- ^ function
                [Term phase] -- ^ arguments
  | Tuple (Context phase)
          (Map Int (Term phase))
  | Record (Context phase)
           Extent
           (Map Keyword (Term phase))
  | Variant (Context phase)
            Extent
            Keyword
            (Term phase)
  | Vector (Context phase)
           [Term phase]
  | Symbol (Context phase)
           Value
  | Atom (Context phase)
         Atom
  | Fix (Context phase)
        Value
        (Term phase)
  | Annotation (Context phase)
               (Term phase)
               (Type phase)
  | Extra (Context phase) (Extra phase)
  deriving (Generic)

deriving instance (Show (Context phase), Show (Extra phase)) => Show (Term phase)

deriving instance (Eq (Context phase), Eq (Extra phase)) => Eq (Term phase)

deriving instance (Ord (Context phase), Ord (Extra phase)) => Ord (Term phase)

instance Tree Term phase where
  walkM f =
          \case
            Lambda ctx args ex -> Lambda ctx <$> pure args <*> down ex
            If ctx test thn els ->
              If ctx <$> down test <*> down thn <*> down els
            Match ctx prototype branches -> do
              prototype' <- down prototype
              branches' <- forM branches $ \Branch{patterns, body} -> do
                body' <- down body
                pure $ Branch{patterns, body = body'}
              pure $ Match ctx prototype' branches'
            Sequence ctx exs -> Sequence ctx <$> traverse down exs
            Let ctx bindings ex ->
              Let ctx <$> traverse (bitraverse pure down) bindings <*> down ex
            Application ctx fn args ->
              Application ctx <$> down fn <*> traverse down args
            Tuple ctx fields -> Tuple ctx <$> traverse down fields
            Record ctx extent rows -> Record ctx extent <$> traverse down rows
            Variant ctx extent tag value -> Variant ctx extent tag <$> down value
            Vector ctx els -> Vector ctx <$> traverse down els
            Symbol ctx name -> pure $ Symbol ctx name
            Atom ctx atom -> pure $ Atom ctx atom
            Fix ctx name body -> Fix ctx name <$> down body
            Annotation ctx term typ -> Annotation ctx <$> down term <*> pure typ
          where
            down = walkM f
  metaM f =
    \case
      Lambda ctx args body -> do
        ctx' <- f ctx
        body' <- metaM f body
        pure $ Lambda ctx' args body'
      If ctx test thn els -> do
        ctx' <- f ctx
        test' <- metaM f test
        thn' <- metaM f thn
        els' <- metaM f els
        pure $ If ctx' test' thn' els'
      Match ctx prototype branches -> do
        ctx' <- f ctx
        prototype' <- metaM f prototype
        branches' <- for branches $ \Branch{patterns, body} -> do
          patterns' <- traverse (metaM f) patterns
          body' <- metaM f body
          pure $ Branch {patterns = patterns', body = body'}
        pure $ Match ctx' prototype' branches'
      Sequence ctx terms -> do
        ctx' <- f ctx
        terms' <- traverse (metaM f) terms
        pure $ Sequence ctx' terms'
      Let ctx bindings body -> do
        ctx' <- f ctx
        bindings' <- traverse (sequenceA . second (metaM f)) bindings
        body' <- metaM f body
        pure $ Let ctx' bindings' body'
      Application ctx fn args -> do
        ctx' <- f ctx
        fn' <- metaM f fn
        args' <- traverse (metaM f) args
        pure $ Application ctx' fn' args'
      Tuple ctx fields -> do
        ctx' <- f ctx
        fields' <- traverse (metaM f) fields
        pure $ Tuple ctx' fields'
      Record ctx extent rows -> do
        ctx' <- f ctx
        rows' <- traverse (metaM f) rows
        pure $ Record ctx' extent rows'
      Variant ctx extent tag value -> do
        ctx' <- f ctx
        value' <- metaM f value
        pure $ Variant ctx' extent tag value'
      Vector ctx els -> do
        ctx' <- f ctx
        els' <- traverse (metaM f) els
        pure $ Vector ctx' els'
      Symbol ctx name -> do
        ctx' <- f ctx
        pure $ Symbol ctx' name
      Atom ctx atom -> do
        ctx' <- f ctx
        pure $ Atom ctx' atom
      Fix ctx name body -> do
        ctx' <- f ctx
        body' <- metaM f body
        pure $ Fix ctx' name body'
      Annotation ctx term typ -> do
        ctx' <- f ctx
        term' <- metaM f term
        typ' <- metaM f typ
        pure $ Annotation ctx' term' typ'


data Branch phase
  = Branch { patterns :: [Pattern phase], body :: Term phase} deriving (Generic)

deriving instance (Show (Context phase), Show (Extra phase)) => Show (Branch phase)

deriving instance (Eq (Context phase), Eq (Extra phase)) => Eq (Branch phase)

deriving instance (Ord (Context phase), Ord (Extra phase)) => Ord (Branch phase)
