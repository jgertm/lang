module Syntax
  ( Name
  , Definition(..)
  , TypeExprA(..)
  , TypeExpr
  , ExprA(..)
  , Expr
  , Pattern(..)
  , Atom(..)
  , descendM
  , descend
  , metaM
  , meta
  ) where

import           Data.Text    (Text)
import           GHC.Generics (Generic)

type Name = Text

data Definition ann
  = DModule [ann]
            Name
            [Definition ann]
  | DMacro [ann]
           Name
           [Name] -- ^ arguments
           MacroExpr
  -- | DClass -- TODO: typeclass definition
  -- | DInstance -- TODO: typeclass instance definition
  | DType [ann]
          Name
          (TypeExprA ann)
  | DConstant [ann]
              Name
              (ExprA ann)
  | DFunction [ann]
              Name
              [Name] -- ^ arguments
              (ExprA ann)
  deriving (Show, Eq)

type MacroExpr = () -- TODO

data TypeExprA ann
  = TProduct [ann]
             [TypeExprA ann]
  | TSum [ann]
         [TypeExprA ann]
  | TRecord [ann]
            [(Name, TypeExprA ann)]
  | TTag [ann]
         Name -- ^ keyword
         (TypeExprA ann)
  | TFunction [ann]
              [TypeExprA ann]
  deriving (Show, Eq)

type TypeExpr = TypeExprA ()

data ExprA ann
  = ELambda [ann]
            Name
            (ExprA ann)
  | EIf [ann]
        (ExprA ann) -- ^ test
        (ExprA ann) -- ^ true
        (ExprA ann) -- ^ false
  | EMatch [ann]
           (ExprA ann) -- ^ prototype
           [(Pattern, ExprA ann)] -- ^ match clauses
  | ESequence [ann]
              [ExprA ann]
  | ELet [ann]
         Name -- ^ bound name
         (ExprA ann) -- ^ body
         (ExprA ann) -- ^ scope
  | EApplication [ann]
                 (ExprA ann) -- ^ function
                 (ExprA ann) -- ^ argument
  | ERecord [ann]
            [(Name, ExprA ann)]
  | EVector [ann]
            [ExprA ann]
  | ESymbol [ann]
            Name
  | EAtom [ann]
          Atom
  deriving (Show, Eq, Ord, Functor, Generic)

type Expr = ExprA ()

data Pattern
  = PSymbol Name
  -- | PTag Name [Pattern]
  | PVector [Pattern]
  | PAtom Atom
  | PWildcard
  deriving (Show, Eq, Ord)

data Atom
  = AUnit
  | AInteger Int
  | AString Text
  | AKeyword Name
  | ABoolean Bool
  -- | AVector [Atom] -- FIXME: this isn't atomic
  | AClosure Expr
             Name
  deriving (Show, Eq, Ord)

descendM ::
     (Monad m) => (ExprA ann -> m (ExprA ann)) -> ExprA ann -> m (ExprA ann)
descendM f ex =
  f =<<
  case ex of
    ELambda m n ex -> ELambda m n <$> descendM f ex
    EIf m test thn els ->
      EIf m <$> descendM f test <*> descendM f thn <*> descendM f els
    EMatch m sample matches ->
      EMatch m <$> descendM f sample <*>
      traverse (sequenceA . second (descendM f)) matches
    ESequence m exs -> ESequence m <$> traverse (descendM f) exs
    ELet m name body ex -> ELet m name <$> descendM f body <*> descendM f ex
    EApplication m fn arg -> EApplication m <$> descendM f fn <*> descendM f arg
    ERecord m rows ->
      ERecord m <$> traverse (sequenceA . second (descendM f)) rows
    EVector m els -> EVector m <$> traverse (descendM f) els
    ESymbol m n -> pure $ ESymbol m n
    EAtom m atom -> pure $ EAtom m atom

descend :: (ExprA ann -> ExprA ann) -> ExprA ann -> ExprA ann
descend f = runIdentity . descendM (pure . f)

metaM :: (Monad m) => ([ann] -> m [bnn]) -> ExprA ann -> m (ExprA bnn)
metaM f ex =
  case ex of
    ELambda m n ex -> ELambda <$> f m <*> pure n <*> metaM f ex
    EIf m test thn els ->
      EIf <$> f m <*> metaM f test <*> metaM f thn <*> metaM f els
    EMatch m sample matches ->
      EMatch <$> f m <*> metaM f sample <*>
      traverse (sequenceA . second (metaM f)) matches
    ESequence m exs -> ESequence <$> f m <*> traverse (metaM f) exs
    ELet m name body ex ->
      ELet <$> f m <*> pure name <*> metaM f body <*> metaM f ex
    EApplication m fn arg -> EApplication <$> f m <*> metaM f fn <*> metaM f arg
    ERecord m rows ->
      ERecord <$> f m <*> traverse (sequenceA . second (metaM f)) rows
    EVector m els -> EVector <$> f m <*> traverse (metaM f) els
    ESymbol m n -> ESymbol <$> f m <*> pure n
    EAtom m atom -> EAtom <$> f m <*> pure atom

meta :: ([ann] -> [bnn]) -> ExprA ann -> ExprA bnn
meta f = runIdentity . metaM (pure . f)
