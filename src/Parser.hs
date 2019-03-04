module Parser
  ( parse
  , Parser
  , Parser.name
  , Parser.moduleName
  , file
  , definition
  , typeExpr
  , term
  , patternExpr
  , atom
  , cases
  )
where

import qualified Data.Map.Strict               as Map
import           Text.Parsec             hiding ( State
                                                , many
                                                , parse
                                                , (<|>)
                                                )
import qualified Text.Parsec                   as P
import           Text.Parsec.Token       hiding ( identifier
                                                , reserved
                                                )
import qualified Text.Parsec.Token             as P

import           Classes
import           Error
import qualified Syntax.Atom                   as Atom
import qualified Syntax.Definition             as Definition
import qualified Syntax.Pattern                as Pattern
import qualified Syntax.Reference              as Reference
import qualified Syntax.Term                   as Term
import qualified Syntax.Type                   as Type


data Parsing

type instance Context Parsing = (SourcePos, SourcePos)
type instance Extra Parsing = ()

type Term = Term.Term Parsing
type Definition = Definition.Definition Parsing
type Type = Type.Type Parsing
type Pattern = Pattern.Pattern Parsing
type Atom = Atom.Atom

parse :: Parser a -> String -> Text -> Either Error a
parse parser sourcePath input = first Parsing $ P.parse parser sourcePath input

type State = ()

type Parser a = ParsecT Text State Identity a

lang :: (Monad m) => GenLanguageDef Text State m
lang = LanguageDef
  { commentStart    = mempty -- ^ Describes the start of a block comment. Use the empty string if the language doesn't support block comments. For example "/*".
  , commentEnd      = mempty -- ^ Describes the end of a block comment. Use the empty string if the language doesn't support block comments. For example "*/".
  , commentLine     = ";" -- ^ Describes the start of a line comment. Use the empty string if the language doesn't support line comments. For example "//".
  , nestedComments  = False -- ^ Set to True if the language supports nested block comments.
  , identStart      = choice [letter, oneOf "*&^%$!_-+=<>?/"] -- ^ This parser should accept any start characters of identifiers. For example letter <|> char '_'.
  , identLetter     = choice [letter, oneOf "*&^%$!_-+=<>?"] -- ^ This parser should accept any legal tail characters of identifiers. For example alphaNum <|> char '_'.
  , opStart         = fail "no ops" -- ^ This parser should accept any start characters of operators. For example oneOf ":!#$%&*+./<=>?@\\^|-~"
  , opLetter        = fail "no ops" -- ^ This parser should accept any legal tail characters of operators. Note that this parser should even be defined if the language doesn't support user-defined operators, or otherwise the reservedOp parser won't work correctly.
  , reservedNames   = mempty -- ^ The list of reserved identifiers.
  , reservedOpNames = mempty -- ^ The list of reserved operators.
  , caseSensitive   = True -- ^ Set to True if the language is case sensitive.
  }

lexer :: (Monad m) => GenTokenParser Text State m
lexer = makeTokenParser lang

ws :: Parser ()
ws = whiteSpace lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer

trimr :: Parser a -> Parser a
trimr p = p <* ws

cases :: [Parser a] -> Parser a
cases = trimr . choice . map P.try

sexp, vector, record, tuple, variant :: Parser a -> Parser a
sexp = parens lexer

vector = brackets lexer

record = braces lexer

tuple = braces lexer

variant = brackets lexer

identifier :: Parser Text
identifier = toText <$> P.identifier lexer

name :: Parser Reference.Value
name = cases [qualifiedName, localName]
 where
  qualifiedName = do
    modul <- moduleName
    char '/'
    Reference.Remote modul <$> identifier
  localName = Reference.Local <$> identifier

moduleName :: Parser Reference.Module
moduleName = Reference.Module <$> identifier `sepBy1` char '.'

typeName :: Parser Reference.Type
typeName = do
  lookAhead upper
  Reference.Type <$> identifier

keyword :: Parser Reference.Keyword
keyword = do
  char ':'
  Reference.Keyword <$> name

injectContext :: Parser (Context Parsing -> ast) -> Parser ast
injectContext p = do
  start  <- getPosition
  result <- p
  end    <- getPosition
  let extent = (start, end)
  pure $ result extent

file :: Parser Definition
file = do
  ws
  (Definition.Module ctx moduleName []) <- definition
  definitions                           <- many definition
  pure $ Definition.Module ctx moduleName definitions

definition :: Parser Definition
definition = injectContext
  $ cases [moduleDefinition, typeDefinition, constantDefinition, functionDefinition]
 where
  moduleDefinition = sexp $ do
    reserved "defmodule"
    modulename <- moduleName
    forms      <- many definition
    pure $ \ctx -> Definition.Module ctx modulename forms
  typeDefinition = sexp $ do
    reserved "deftype"
    typename  <- typeName
    params    <- many typeName
    structure <- typeExpr
    pure $ \ctx -> Definition.Type ctx typename params structure
  constantDefinition = sexp $ do
    reserved "def"
    constname <- name
    value     <- term
    pure $ \ctx -> Definition.Constant ctx constname value
  functionDefinition = sexp $ do
    reserved "defn"
    funcname <- name
    args     <- vector $ many name
    body     <- many1 term
    pure $ \ctx ->
      let body' = case body of
            [form] -> form
            forms  -> Term.Sequence ctx forms
      in  Definition.Constant ctx funcname $ Term.Fix ctx funcname $ foldr' (Term.Lambda ctx)
                                                                            body'
                                                                            args
typeExpr :: Parser Type
typeExpr = injectContext
  $ cases [namedType, applicationType, tupleType, recordType, variantType, functionType]
 where
  namedType = do
    name <- typeName
    pure $ \ctx -> Type.Named ctx name
  applicationType = sexp $ do
    hkt    <- typeName
    params <- many1 typeExpr
    pure $ \ctx -> Type.Application ctx hkt params
  tupleType = tuple $ do
    elements <- many1 typeExpr
    pure $ \ctx -> Type.Tuple ctx elements
  recordType = record $ do
    rows <- record (many1 row)
    pure $ \ctx -> Type.Record ctx rows
    where row = (,) <$> keyword <*> typeExpr
  variantType = sexp $ do
    reserved "|"
    variants <- many1 $ variant $ (,) <$> keyword <*> typeExpr
    pure $ \ctx -> Type.Variant ctx variants
  functionType = sexp $ do
    reserved "fn"
    domains <- many1 typeExpr
    pure $ \ctx -> Type.Function ctx domains

term :: Parser Term
term = injectContext $ cases
  [ lambdaTerm
  , ifTerm
  , matchTerm
  , sequenceTerm
  , letTerm
  , fixTerm
  , applicationTerm
  , tupleTerm
  , recordTerm
  , variantTerm
  , vectorTerm
  , atomTerm
  , symbolTerm
  ]
 where
  lambdaTerm = sexp $ do
    reserved "fn"
    args <- vector $ many name
    body <- many term
    pure $ \ctx ->
      let body' = case body of
            [form] -> form
            forms  -> Term.Sequence ctx forms
      in  foldr' (Term.Lambda ctx) body' args
  ifTerm = sexp $ do
    reserved "if"
    test <- term
    thn  <- term
    els  <- term
    pure $ \ctx -> Term.If ctx test thn els
  matchTerm = sexp $ do
    reserved "match"
    body     <- term
    branches <- many1 branch
    pure $ \ctx -> Term.Match ctx body branches
   where
    branch = sexp $ do
      patterns <- alternatives <|> single
      body     <- term
      pure $ Term.Branch {patterns , body }
    single       = one <$> patternExpr
    alternatives = sexp $ do
      reserved "|"
      many1 patternExpr
  sequenceTerm = sexp $ do
    reserved "do"
    steps <- many1 term
    pure $ \ctx -> Term.Sequence ctx steps
  letTerm = sexp $ do
    reserved "let"
    bindings <- vector $ many association
    body     <- term
    pure $ \ctx -> Term.Let ctx bindings body
    where association = vector $ (,) <$> name <*> term
  applicationTerm = sexp $ do
    function <- term
    args     <- many term
    pure $ \ctx -> Term.Application ctx function args
  tupleTerm = tuple $ do
    fields <- Map.fromList . zip [1 ..] <$> many1 term
    pure $ \ctx -> Term.Tuple ctx fields
  recordTerm = record $ do
    rows <- Map.fromList <$> many row
    pure $ \ctx -> Term.Record ctx rows
    where row = (,) <$> keyword <*> term
  variantTerm = variant $ do
    tag  <- keyword
    body <- term
    pure $ \ctx -> Term.Variant ctx tag body
  vectorTerm = vector $ do
    elements <- many term
    pure $ \ctx -> Term.Vector ctx elements
  symbolTerm = do
    sym <- name
    pure $ \ctx -> Term.Symbol ctx sym
  atomTerm = do
    a <- atom
    pure $ \ctx -> Term.Atom ctx a
  fixTerm = sexp $ do
    reserved "recur"
    name <- name
    body <- term
    pure $ \ctx -> Term.Fix ctx name body

patternExpr :: Parser Pattern
patternExpr = injectContext $ cases
  [ wildcardPattern
  , vectorPattern
  , tuplePattern
  , recordPattern
  , variantPattern
  , atomPattern
  , symbolPattern
  ]
 where
  wildcardPattern = reserved "_" $> \ctx -> Pattern.Wildcard ctx
  symbolPattern   = do
    sym <- name
    pure $ \ctx -> Pattern.Symbol ctx sym
  vectorPattern = vector $ do
    subpatterns <- many patternExpr
    pure $ \ctx -> Pattern.Vector ctx subpatterns
  tuplePattern = tuple $ do
    subpatterns <- Map.fromList . zip [1 ..] <$> many1 patternExpr
    pure $ \ctx -> Pattern.Tuple ctx subpatterns
  recordPattern = record $ do
    subpatterns <- Map.fromList <$> many row
    pure $ \ctx -> Pattern.Record ctx subpatterns
    where row = (,) <$> keyword <*> patternExpr
  variantPattern = variant $ do
    tag        <- keyword
    subpattern <- patternExpr
    pure $ \ctx -> Pattern.Variant ctx tag subpattern
  atomPattern = do
    atm <- atom
    pure $ \ctx -> Pattern.Atom ctx atm

atom :: Parser Atom
atom = cases [unitAtom, integerAtom, stringAtom, booleanAtom]
 where
  unitAtom = do
    reserved "nil"
    pure Atom.Unit
  integerAtom = do
    mnum <- readMaybe <$> many1 digit
    case mnum of
      Nothing  -> parserFail "couldn't decipher integer"
      Just num -> pure $ Atom.Integer num
  stringAtom  = Atom.String . toText <$> between (char '"') (char '"') (many $ noneOf "\"")
  booleanAtom = Atom.Boolean <$> choice [reserved "true" $> True, reserved "false" $> False]
