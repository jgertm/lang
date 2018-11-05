{-# LANGUAGE UndecidableInstances #-}

module Syntax
  ( Name
  , Context
  , Tree(..)
  , ascend
  , descend
  , meta
  , context
  , Definition'(..)
  , nameOf
  , Type'(..)
  , Term'(..)
  , Binding(..)
  , Pattern'(..)
  , Atom(..)
  , Builtin(..)
  , showAtom
  )
where

import           Data.Bitraversable
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           GHC.Show                       ( Show(showsPrec) )
import qualified Universum.Unsafe              as Unsafe

import           Classes
import           Types                          ( Type )

type Name = Text

data Definition' phase
  = DModule (Context phase)
            Binding
            [Definition' phase]
  | DMacro (Context phase)
           Binding
           [Binding] -- ^ arguments
           Macro
  -- | DClass -- TODO: typeclass definition
  -- | DInstance -- TODO: typeclass instance definition
  | DType (Context phase)
          Binding
          (Type' phase)
  | DConstant (Context phase)
              Binding
              (Term' phase)
  | DFunction (Context phase)
              Binding
              [Binding] -- ^ arguments
              (Term' phase) -- ^ body
  deriving (Generic)

deriving instance
         (Show (Context phase)) => Show (Definition' phase)

deriving instance (Eq (Context phase)) => Eq (Definition' phase)

deriving instance (Ord (Context phase)) => Ord (Definition' phase)

instance Tree Definition' phase where
  walkM dir f =
    let step =
          \case
            DModule ctx name defs ->
              DModule ctx name <$> traverse (walkM dir f) defs
     in case dir of
          Up   -> f <=< step
          Down -> step <=< f
  metaM f def =
    case def of
      DModule ctx name defs -> do
        ctx' <- f ctx
        defs' <- traverse (metaM f) defs
        pure $ DModule ctx' name defs'
      DConstant ctx name term -> do
        ctx' <- f ctx
        term' <- metaM f term
        pure $ DConstant ctx' name term'
      DFunction ctx name args body -> do
        ctx' <- f ctx
        body' <- metaM f body
        pure $ DFunction ctx' name args body'

nameOf :: Definition' phase -> Binding
nameOf (DModule   _ n _  ) = n
nameOf (DConstant _ n _  ) = n
nameOf (DFunction _ n _ _) = n

type Macro = () -- TODO

data Type' phase
  = TProduct (Context phase)
             [Type' phase]
  | TSum (Context phase)
         [Type' phase]
  | TRecord (Context phase)
            [(Name, Type' phase)]
  | TTag (Context phase)
         Name -- ^ keyword
         (Type' phase)
  | TFunction (Context phase)
              [Type' phase]
  deriving (Generic)

deriving instance (Show (Context phase)) => Show (Type' phase)

deriving instance (Eq (Context phase)) => Eq (Type' phase)

deriving instance (Ord (Context phase)) => Ord (Type' phase)

data Term' phase
  = ELambda (Context phase)
            [Binding] -- ^ arguments
            (Term' phase) -- ^ body
  | EIf (Context phase)
        (Term' phase) -- ^ test
        (Term' phase) -- ^ true
        (Term' phase) -- ^ false
  | EMatch (Context phase)
           (Term' phase) -- ^ prototype
           [(Pattern' phase, Term' phase)] -- ^ match clauses
  | ESequence (Context phase)
              [Term' phase]
  | ELet (Context phase)
         [( Binding -- ^ bound name
          , Term' phase -- ^ body
           )]
         (Term' phase) -- ^ scope
  | EApplication (Context phase)
                 (Term' phase) -- ^ function
                 [Term' phase] -- ^ arguments
  | ERecord (Context phase)
            [(Name, Term' phase)]
  | EVector (Context phase)
            [Term' phase]
  | ESymbol (Context phase)
            Binding
  | EKeyword (Context phase)
             Name
  | EAtom (Context phase)
          Atom
  -- | TODO: replace this with a generic, phase-specific constructor,
  -- only include native constructor in evaluation stage
  | ENative (Context phase)
            Builtin
            [Atom]
  -- | EFix (Context phase) (Term' phase) -- TODO: need this for typing recursive functions
  deriving (Generic)

deriving instance (Show (Context phase)) => Show (Term' phase)

deriving instance (Eq (Context phase)) => Eq (Term' phase)

deriving instance (Ord (Context phase)) => Ord (Term' phase)

instance Tree Term' phase where
  walkM dir f =
    let step =
          \case
            ELambda ctx args ex -> ELambda ctx <$> pure args <*> down ex
            EIf ctx test thn els ->
              EIf ctx <$> down test <*> down thn <*> down els
            EMatch ctx sample matches ->
              EMatch ctx <$> down sample <*>
              traverse (sequenceA . second down) matches
            ESequence ctx exs -> ESequence ctx <$> traverse down exs
            ELet ctx bindings ex ->
              ELet ctx <$> traverse (bitraverse pure down) bindings <*> down ex
            EApplication ctx fn args ->
              EApplication ctx <$> down fn <*> traverse down args
            ERecord ctx rows ->
              ERecord ctx <$> traverse (sequenceA . second down) rows
            EVector ctx els -> EVector ctx <$> traverse down els
            ESymbol ctx n -> pure $ ESymbol ctx n
            EAtom ctx atom -> pure $ EAtom ctx atom
          where
            down = walkM dir f
     in case dir of
          Up   -> f <=< step
          Down -> step <=< f
  metaM f =
    \case
      ELambda ctx args body -> do
        ctx' <- f ctx
        body' <- metaM f body
        pure $ ELambda ctx' args body'
      EIf ctx test thn els -> do
        ctx' <- f ctx
        test' <- metaM f test
        thn' <- metaM f thn
        els' <- metaM f els
        pure $ EIf ctx' test' thn' els'
      EMatch ctx sample matches -> do
        ctx' <- f ctx
        sample' <- metaM f sample
        matches' <- traverse (bitraverse (metaM f) (metaM f)) matches
        pure $ EMatch ctx' sample' matches'
      ESequence ctx terms -> do
        ctx' <- f ctx
        terms' <- traverse (metaM f) terms
        pure $ ESequence ctx' terms'
      ELet ctx bindings body -> do
        ctx' <- f ctx
        bindings' <- traverse (sequenceA . second (metaM f)) bindings
        body' <- metaM f body
        pure $ ELet ctx' bindings' body'
      EApplication ctx fn args -> do
        ctx' <- f ctx
        fn' <- metaM f fn
        args' <- traverse (metaM f) args
        pure $ EApplication ctx' fn' args'
      ERecord ctx rows -> do
        ctx' <- f ctx
        rows' <- traverse (sequenceA . second (metaM f)) rows
        pure $ ERecord ctx' rows'
      EVector ctx els -> do
        ctx' <- f ctx
        els' <- traverse (metaM f) els
        pure $ EVector ctx' els'
      ESymbol ctx name -> do
        ctx' <- f ctx
        pure $ ESymbol ctx' name
      EAtom ctx atom -> do
        ctx' <- f ctx
        pure $ EAtom ctx' atom

newtype Binding =
  Single Name
  deriving (Show, Eq, Ord)

data Pattern' phase
  = PWildcard (Context phase)
  | PSymbol (Context phase)
            Binding
  | PVector (Context phase)
            [Pattern' phase]
  -- | PRecord (Context phase) -- TODO
  -- | PVariant (Context phase) -- TODO
  | PAtom (Context phase)
          Atom
  deriving (Generic)

deriving instance (Show (Context phase)) => Show (Pattern' phase)

deriving instance (Eq (Context phase)) => Eq (Pattern' phase)

deriving instance (Ord (Context phase)) => Ord (Pattern' phase)

instance Tree Pattern' phase where
  walkM dir f =
    let step =
          \case
            (PVector ctx ps) -> PVector ctx <$> traverse (walkM dir f) ps
            p -> pure p
     in case dir of
          Up   -> f <=< step
          Down -> step <=< f
  metaM f pat =
    case pat of
      PWildcard ctx    -> PWildcard <$> f ctx
      PSymbol ctx bind -> PSymbol <$> f ctx <*> pure bind
      PVector ctx ps   -> PVector <$> f ctx <*> traverse (metaM f) ps
      PAtom ctx atom   -> PAtom <$> f ctx <*> pure atom

data Atom
  = AUnit
  | AInteger Int
  | AString Text
  | ABoolean Bool
  deriving (Show, Eq, Ord, Generic)

showAtom :: (IsString s) => Atom -> s
showAtom AUnit        = "nil"
showAtom (AInteger i) = show i
showAtom (AString  s) = show s
showAtom (ABoolean b) = if b then "true" else "false"

data Builtin = Builtin
  { name     :: Name
  , function :: [Atom] -> Atom
  , typ      :: Type
  }

instance Show Builtin where
  showsPrec i = showsPrec i . name

instance Eq Builtin where
  x == y = name x == name y

instance Ord Builtin where
  x `compare` y = name x `compare` name y
