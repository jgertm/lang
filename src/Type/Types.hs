module Type.Types
  ( Type(..)
  , Fact(..)
  , Context(..)
  , Kind(..)
  , Variable(..)
  , Quantification(..)
  , Row(..)
  , Principality(..)
  , Proposition(..)
  , Polarity(..)
  , Term
  , Atom
  , Branch
  , Pattern
  )
where

import           Data.Text.Prettyprint.Doc

import qualified Classes
import qualified Syntax.Atom                   as Syntax
import qualified Syntax.Pattern                as Syntax
import qualified Syntax.Reference              as Syntax
import qualified Syntax.Term                   as Syntax


type Term = Syntax.Term Classes.Empty
type Atom = Syntax.Atom
type Pattern = Syntax.Pattern Classes.Empty
type Branch = Syntax.Branch Classes.Empty

newtype Context = Context
  { unContext :: Seq Fact
  } deriving (Show, Eq, Ord)

data Fact
  = DeclareUniversal (Variable 'Universal)
                     Kind
  | DeclareExistential (Variable 'Existential)
                       Kind
  | Binding Syntax.Value
            Type
            Principality
  | SolvedUniversal (Variable 'Universal)
                    Type
  | SolvedExistential (Variable 'Existential)
                      Kind
                      Type
  | Marker (Variable 'Existential)
  deriving (Show, Ord, Generic)

instance Eq Fact where
  (==) (DeclareUniversal name kind) = \case
    DeclareUniversal name' kind' -> name == name' && kind == kind'
    SolvedUniversal name' _ -> name == name'
    _ -> False
  (==) (DeclareExistential name kind) = \case
    DeclareExistential name' kind' -> name == name' && kind == kind'
    SolvedExistential name' kind' _ -> name == name' && kind == kind'
    _ -> False
  (==) (SolvedUniversal name _) = \case
    SolvedUniversal name' _ -> name == name'
    DeclareUniversal name' _ -> name == name'
    _ -> False
  (==) (SolvedExistential name kind _) = \case
    DeclareExistential name' kind' -> name == name' && kind == kind'
    SolvedExistential name' kind' _ -> name == name' && kind == kind'
    _ -> False
  (==) (Binding name _ _) = \case
    Binding name' _ _ -> name == name'
    _ -> False
  (==) (Marker name) = \case
    Marker name' -> name == name'
    _ -> False

data Principality
  = Principal
  | Nonprincipal
  deriving (Show, Eq, Ord, Generic)

data Quantification
  = Existential
  | Universal

newtype Variable (quantification :: Quantification) =
  Var Text
  deriving (Show, Eq, Ord, Generic)

data Kind
  = Type
  | Natural
  deriving (Show, Eq, Ord, Generic)

data Type
  = Primitive Text
  | Function Type Type
  | Variant Row (Map Syntax.Keyword Type)
  | Tuple (Map Int Type)
  | Record Row (Map Syntax.Keyword Type)
  | UniversalVariable (Variable 'Universal)
  | ExistentialVariable (Variable 'Existential)
  | Forall (Variable 'Universal)
           Kind
           Type
  | Exists (Variable 'Universal)
           Kind
           Type
  | Implies Proposition
            Type
  | With Type
         Proposition
  | Zero
  | Succ Type
  | Vector Type
           Type
  | Fix (Variable 'Existential)
        Type
  deriving (Show, Eq, Ord, Generic)

instance Pretty Type where
  pretty (Primitive prim) = pretty prim
  pretty (Function a b) =
    let args (Function a b) = a : args b
        args t              = [t]
        body = hsep $ "->" : map pretty (a : args b)
    in parens body
  pretty (Variant rowvar cases) =
    let cases' = map (\(tag, typ) -> brackets $ pretty tag <+> pretty typ) $ toPairs cases
        suffix = case rowvar of
                   Closed -> mempty
                   Open _ -> "..."
    in  parens $ (hsep $ "|" : cases') <+> suffix
  pretty (Tuple fields) = braces $ hsep $ map pretty $ elems fields
  pretty (Record rowvar fields) =
    let fields' = map (\(name, typ) -> pretty name <+> pretty typ) $ toPairs fields
        suffix = case rowvar of
                   Closed -> mempty
                   Open _ -> "..."
    in  braces $ (hsep fields') <+> suffix
  pretty (UniversalVariable (Var var)) = pretty var
  pretty (ExistentialVariable (Var var)) = pretty var
  pretty (Forall (Var var) _ typ) =
    parens $ hsep ["forall", pretty var <> ".", pretty typ]
  pretty (Fix (Var var) typ) =
    parens $ hsep ["fix", pretty var <> ".", pretty typ ]
  pretty t = show t

data Row = Closed | Open (Variable 'Existential) deriving (Generic, Show, Eq, Ord)

data Proposition =
  Equals Type
         Type
  deriving (Show, Eq, Ord, Generic)

data Polarity
  = Positive
  | Negative
  | Neutral
  deriving (Show, Eq)
