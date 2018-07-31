module Syntax.Abstract
  ( Name
  , Definition(..)
  , TypeExpr(..)
  , ValueExpr(..)
  , PatternExpr(..)
  , Atom(..)
  , Env
  , descendM
  , descend
  ) where

import           Data.Bifunctor
import           Data.Functor.Identity
import           Data.Map              (Map)
import           Data.Text             (Text)

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
              ValueExpr
  | DFunction Name
              [Name] -- ^ arguments
              ValueExpr
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

data ValueExpr
  = VLambda Name
            ValueExpr
  | VIf ValueExpr -- ^ test
        ValueExpr -- ^ true
        ValueExpr -- ^ false
  | VMatch ValueExpr -- ^ prototype
           [(PatternExpr, ValueExpr)] -- ^ match clauses
  | VSequence [ValueExpr]
  | VLet Name -- ^ bound name
         ValueExpr -- ^ body
         ValueExpr -- ^ scope
  | VApplication ValueExpr -- ^ function
                 ValueExpr -- ^ argument
  | VRecord [(Name, ValueExpr)]
  | VVector [ValueExpr]
  | VSymbol Name
  | VAtom Atom
  deriving (Show, Eq, Ord)

data PatternExpr
  = PSymbol Name
  -- | PTag Name [PatternExpr]
  | PVector [PatternExpr]
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
  | AClosure ValueExpr
             Name
  deriving (Show, Eq, Ord)

type Env = Map Name Atom

descendM :: (Monad m) => (ValueExpr -> m ValueExpr) -> ValueExpr -> m ValueExpr
descendM f expr =
  f =<<
  case expr of
    VLambda n ex -> VLambda n <$> descendM f ex
    VIf test thn els ->
      VIf <$> descendM f test <*> descendM f thn <*> descendM f els
    VMatch sample matches ->
      VMatch <$> descendM f sample <*>
      traverse (sequenceA . second (descendM f)) matches
    VSequence exs -> VSequence <$> traverse (descendM f) exs
    VLet name body ex -> VLet name <$> descendM f body <*> descendM f ex
    VApplication n arg -> VApplication n <$> descendM f arg
    VRecord rows -> VRecord <$> traverse (sequenceA . second (descendM f)) rows
    VVector els -> VVector <$> traverse (descendM f) els
    VSymbol n -> pure $ VSymbol n
    VAtom atom -> pure $ VAtom atom

descend :: (ValueExpr -> ValueExpr) -> ValueExpr -> ValueExpr
descend f = runIdentity . descendM (pure . f)
