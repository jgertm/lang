{-# LANGUAGE UndecidableInstances #-}

module Syntax.Term where

import           Data.Bitraversable

import           Classes
import           Syntax.Atom                    ( Atom )
import           Syntax.Common                  ( Binding
                                                , Name
                                                )
import           Syntax.Pattern                 ( Pattern )
import           Syntax.Type                    ( Type )


data Term phase
  = Lambda (Context phase)
           Binding -- ^ argument name
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
        [( Binding -- ^ bound name
         , Term phase -- ^ body
          )]
        (Term phase) -- ^ scope
  | Application (Context phase)
                (Term phase) -- ^ function
                [Term phase] -- ^ arguments
  | Tuple (Context phase)
          (Map Int (Term phase))
  | Record (Context phase)
           (Map Name (Term phase))
  | Variant (Context phase)
            Name
            (Term phase)
  | Vector (Context phase)
           [Term phase]
  | Symbol (Context phase)
           Binding
  | Keyword (Context phase)
            Name
  | Atom (Context phase)
         Atom
  | Fix (Context phase)
        Binding
        (Term phase)
  | Annotation (Context phase)
               (Term phase)
               (Type phase)
  deriving (Generic)

type Branch phase = ([Pattern phase], Term phase)


deriving instance (Show (Context phase)) => Show (Term phase)

deriving instance (Eq (Context phase)) => Eq (Term phase)

deriving instance (Ord (Context phase)) => Ord (Term phase)

instance Tree Term phase where
  walkM dir f =
    let step =
          \case
            Lambda ctx args ex -> Lambda ctx <$> pure args <*> down ex
            If ctx test thn els ->
              If ctx <$> down test <*> down thn <*> down els
            Match ctx sample branches ->
              Match ctx <$> down sample <*>
              traverse (sequenceA . second down) branches
            Sequence ctx exs -> Sequence ctx <$> traverse down exs
            Let ctx bindings ex ->
              Let ctx <$> traverse (bitraverse pure down) bindings <*> down ex
            Application ctx fn args ->
              Application ctx <$> down fn <*> traverse down args
            Tuple ctx fields -> Tuple ctx <$> traverse down fields
            Record ctx rows -> Record ctx <$> traverse down rows
            Variant ctx tag value -> Variant ctx tag <$> down value
            Vector ctx els -> Vector ctx <$> traverse down els
            Symbol ctx name -> pure $ Symbol ctx name
            Keyword ctx name -> pure $ Keyword ctx name
            Atom ctx atom -> pure $ Atom ctx atom
            Fix ctx name body -> Fix ctx name <$> down body
            Annotation ctx term typ -> Annotation ctx <$> down term <*> pure typ
          where
            down = walkM dir f
     in case dir of
          Up   -> f <=< step
          Down -> step <=< f
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
      Match ctx sample branches -> do
        ctx' <- f ctx
        sample' <- metaM f sample
        branches' <-
          traverse
            (\(patterns, body) -> do
               patterns' <- traverse (metaM f) patterns
               body' <- metaM f body
               pure (patterns', body'))
            branches
        pure $ Match ctx' sample' branches'
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
      Record ctx rows -> do
        ctx' <- f ctx
        rows' <- traverse (metaM f) rows
        pure $ Record ctx' rows'
      Variant ctx tag value -> do
        ctx' <- f ctx
        value' <- metaM f value
        pure $ Variant ctx' tag value'
      Vector ctx els -> do
        ctx' <- f ctx
        els' <- traverse (metaM f) els
        pure $ Vector ctx' els'
      Symbol ctx name -> do
        ctx' <- f ctx
        pure $ Symbol ctx' name
      Keyword ctx name -> do
        ctx' <- f ctx
        pure $ Keyword ctx' name
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
