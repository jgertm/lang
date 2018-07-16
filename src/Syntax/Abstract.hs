module Syntax.Abstract where

import           Data.Text (Text)

type Name = Text

data Definition
  = DModule Name
            [Definition]
  | DMacro Name
           [Name] -- ^ arguments
           MacroExpr
  -- | DClass -- TODO: typeclass definition
  | DType Name
          TypeExpr
  -- | DInstance -- TODO: typeclass instance definition
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
  = VLambda [Name]
            ValueExpr
  | VIf ValueExpr -- ^ test
        ValueExpr -- ^ true
        ValueExpr -- ^ false
  | VMatch ValueExpr -- ^ prototype
           [(Pattern, ValueExpr)] -- ^ match clauses
  | VSequence [ValueExpr]
  | VLet [(Name, ValueExpr)] -- ^ bindings
         ValueExpr -- ^ scope
  | VApplication Name -- ^ function
                 [ValueExpr] -- ^ arguments
  | VRecord [(Name, ValueExpr)]
  | VVector [ValueExpr]
  | VSymbol Name
  | VAtom Atom
  deriving (Show, Eq)

type Pattern = ValueExpr -- TODO

data Atom
  = AUnit
  | AInteger Int
  | AString Text
  | AKeyword Name
  deriving (Show, Eq)
