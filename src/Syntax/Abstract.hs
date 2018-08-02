module Syntax.Abstract
  ( Name
  , Definition(..)
  , TypeExpr(..)
  , ExprA(..)
  , Expr
  , Meta(..)
  , Pattern(..)
  , Atom(..)
  , descendM
  , descend
  ) where

import           Data.Bifunctor
import           Data.Functor.Identity
import           Data.Generics.Product.Typed
import           Data.Set                    (Set)
import           Data.Text                   (Text)
import           GHC.Generics                (Generic)
import           Lens.Micro.Platform
import           Text.Parsec                 (SourcePos)

type Name = Text

data Definition
  = DModule Name
            [Definition]
  | DMacro Name
           [Name] -- ^ arguments
           MacroExpr
  -- | DClass -- TODO: typeclass definition
  -- | DInstance -- TODO: typeclass instance definition
  | DType Name
          TypeExpr
  | DConstant Name
              (ExprA Meta)
  | DFunction Name
              [Name] -- ^ arguments
              (ExprA Meta)
  deriving (Show, Eq)

type MacroExpr = () -- TODO

data TypeExpr
  = TProduct [TypeExpr]
  | TSum [TypeExpr]
  | TRecord [(Name, TypeExpr)]
  | TTag Name -- ^ keyword
         TypeExpr
  | TFunction [TypeExpr]
  deriving (Show, Eq)

data ExprA ann
  = ELambda ann
            Name
            (ExprA ann)
  | EIf ann
        (ExprA ann) -- ^ test
        (ExprA ann) -- ^ true
        (ExprA ann) -- ^ (ExprA ann)alse
  | EMatch ann
           (ExprA ann) -- ^ prototype
           [(Pattern, ExprA ann)] -- ^ match clauses
  | ESequence ann
              [ExprA ann]
  | ELet ann
         Name -- ^ bound name
         (ExprA ann) -- ^ body
         (ExprA ann) -- ^ scope
  | EApplication ann
                 (ExprA ann) -- ^ (ExprA ann)unction
                 (ExprA ann) -- ^ argument
  | ERecord ann
            [(Name, ExprA ann)]
  | EVector ann
            [ExprA ann]
  | ESymbol ann
            Name
  | EAtom ann
          Atom
  deriving (Show, Eq, Ord, Functor, Generic, Foldable, Traversable)

data Meta = Meta
  { extent :: (SourcePos, SourcePos)
  } deriving (Show, Eq, Ord, Generic)

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
    EApplication m n arg -> EApplication m n <$> descendM f arg
    ERecord m rows ->
      ERecord m <$> traverse (sequenceA . second (descendM f)) rows
    EVector m els -> EVector m <$> traverse (descendM f) els
    ESymbol m n -> pure $ ESymbol m n
    EAtom m atom -> pure $ EAtom m atom

descend :: (ExprA ann -> ExprA ann) -> ExprA ann -> ExprA ann
descend f = runIdentity . descendM (pure . f)
