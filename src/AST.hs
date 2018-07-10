module AST where

import           Data.Text (Text)

data Expr
  = Module [Name]
           [Expr]
  -- | Open
  | Type Identifier
         [Expr]
  -- | Macro
  -- | Class
  -- | Instance
  | Annotation Name
               Expr
  -- | Constant Name
  --            Expr
  | Function Name
             [Name]
             [Expr]
  -- | Lambda
  | Application Identifier
                [Expr]
  | Match Expr
          [(Expr, Expr)]
  -- | Let
  | Vector [Expr]
  | Symbol Identifier
  | Quote Expr
  | String Text
  | Integer Integer
  | Keyword Name
  | Unit
  | Comment Text
  deriving (Show, Eq)

data Identifier =
  Identifier (Maybe [Name])
             Name
  deriving (Show, Eq)

newtype Name =
  Name Text
  deriving (Show, Eq)
