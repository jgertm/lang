module Parser
  ( parse
  , Parser
  , Parser.name
  , file
  , definition
  , typeExpr
  , term
  , patternExpr
  , atom
  , cases
  ) where

import           Text.Parsec       hiding (State, many, parse)
import qualified Text.Parsec       as P
import           Text.Parsec.Token hiding (identifier, reserved)
import qualified Text.Parsec.Token as P

import           Error
import           Syntax

data Parsing

type instance Context Parsing = (SourcePos, SourcePos)

type Term = Term' Parsing

type Definition = Definition' Parsing

type Type = Type' Parsing

type Pattern = Pattern' Parsing

parse :: Parser a -> String -> Text -> Either Error a
parse parser source input = first Parsing $ P.parse parser source input

type State = ()

type Parser a = ParsecT Text State Identity a

lang :: (Monad m) => GenLanguageDef Text State m
lang =
  LanguageDef
    { commentStart = mempty -- ^ Describes the start of a block comment. Use the empty string if the language doesn't support block comments. For example "/*".
    , commentEnd = mempty -- ^ Describes the end of a block comment. Use the empty string if the language doesn't support block comments. For example "*/".
    , commentLine = ";" -- ^ Describes the start of a line comment. Use the empty string if the language doesn't support line comments. For example "//".
    , nestedComments = False -- ^ Set to True if the language supports nested block comments.
    , identStart = choice [letter, oneOf "*&^%$!_-+=<>?"] -- ^ This parser should accept any start characters of identifiers. For example letter <|> char '_'.
    , identLetter = choice [letter, oneOf "*&^%$!_-+=<>?./"] -- ^ This parser should accept any legal tail characters of identifiers. For example alphaNum <|> char '_'.
    , opStart = fail "no ops" -- ^ This parser should accept any start characters of operators. For example oneOf ":!#$%&*+./<=>?@\\^|-~"
    , opLetter = fail "no ops" -- ^ This parser should accept any legal tail characters of operators. Note that this parser should even be defined if the language doesn't support user-defined operators, or otherwise the reservedOp parser won't work correctly.
    , reservedNames = mempty -- ^ The list of reserved identifiers.
    , reservedOpNames = mempty -- ^ The list of reserved operators.
    , caseSensitive = True -- ^ Set to True if the language is case sensitive.
    }

lexer :: (Monad m) => GenTokenParser Text State m
lexer = makeTokenParser lang

ws :: Parser ()
ws = whiteSpace lexer

trimr :: Parser a -> Parser a
trimr p = p <* ws

cases :: [Parser a] -> Parser a
cases = trimr . choice . map P.try

sexp, vector, record :: Parser a -> Parser a
sexp = parens lexer

vector = brackets lexer

record = braces lexer

identifier :: Parser Text
identifier = toText <$> P.identifier lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer

injectContext :: Parser (Context Parsing -> ast) -> Parser ast
injectContext p = do
  start <- getPosition
  result <- p
  end <- getPosition
  let extent = (start, end)
  pure $ result extent

name :: Parser Term
name =
  injectContext $ do
    n <- binding
    pure $ \ctx -> ESymbol ctx n

file :: Parser Definition
file = do
  ws
  (DModule ctx moduleName []) <- definition
  definitions <- many definition
  pure $ DModule ctx moduleName definitions

definition :: Parser Definition
definition =
  injectContext $
  cases
    [moduleDefinition, typeDefinition, constantDefinition, functionDefinition]
  where
    moduleDefinition =
      sexp $ do
        reserved "defmodule"
        modulename <- identifier
        forms <- many definition
        pure $ \ctx -> DModule ctx modulename forms
    typeDefinition =
      sexp $ do
        reserved "deftype"
        typename <- identifier
        structure <- typeExpr
        pure $ \ctx -> DType ctx typename structure
    constantDefinition =
      sexp $ do
        reserved "def"
        constname <- identifier
        value <- term
        pure $ \ctx -> DConstant ctx constname value
    functionDefinition =
      sexp $ do
        reserved "defn"
        funcname <- identifier
        args <- vector $ many binding
        body <- many term
        pure $ \ctx ->
          DFunction ctx funcname args $
          case body of
            [form] -> form
            forms  -> ESequence ctx forms

typeExpr :: Parser Type
typeExpr =
  injectContext $
  cases [productType, sumType, recordType, tagType, functionType]
  where
    productType =
      sexp $ do
        reserved "product"
        factors <- many1 typeExpr
        pure $ \ctx -> TProduct ctx factors
    sumType =
      sexp $ do
        reserved "sum"
        summands <- many1 typeExpr
        pure $ \ctx -> TSum ctx summands
    recordType =
      sexp $ do
        reserved "record"
        rows <- record (many1 row)
        pure $ \ctx -> TRecord ctx rows
      where
        row = (,) <$> identifier <*> typeExpr
    tagType =
      sexp $ do
        reserved "tag"
        tagname <- identifier
        inner <- typeExpr
        pure $ \ctx -> TTag ctx tagname inner
    functionType =
      sexp $ do
        reserved "fn"
        domains <- many1 typeExpr
        pure $ \ctx -> TFunction ctx domains

term :: Parser Term
term =
  injectContext $
  cases
    [ lambdaValue
    , ifValue
    , matchValue
    , sequenceValue
    , letValue
    , applicationValue
    , recordValue
    , vectorValue
    , keywordValue
    , atomValue
    , symbolValue
    ]
  where
    lambdaValue =
      sexp $ do
        reserved "fn"
        args <- vector $ many binding
        body <- term
        pure $ \ctx -> ELambda ctx args body
    ifValue =
      sexp $ do
        reserved "if"
        test <- term
        thn <- term
        els <- term
        pure $ \ctx -> EIf ctx test thn els
    matchValue =
      sexp $ do
        reserved "match"
        body <- term
        patterns <- many1 pattrn
        pure $ \ctx -> EMatch ctx body patterns
      where
        pattrn = sexp $ (,) <$> patternExpr <*> term
    sequenceValue =
      sexp $ do
        reserved "do"
        steps <- many1 term
        pure $ \ctx -> ESequence ctx steps
    letValue =
      sexp $ do
        reserved "let"
        bindings <- vector $ many association
        body <- term
        pure $ \ctx -> ELet ctx bindings body
      where
        association = vector $ (,) <$> binding <*> term -- TODO: individual vector for bindings might be unneccesary
    applicationValue =
      sexp $ do
        function <- term
        args <- many term
        pure $ \ctx -> EApplication ctx function args
    recordValue =
      record $ do
        rows <- many1 row
        pure $ \ctx -> ERecord ctx rows
      where
        row = (,) <$> identifier <*> term
    vectorValue =
      vector $ do
        elements <- many term
        pure $ \ctx -> EVector ctx elements
    symbolValue = do
      sym <- binding
      pure $ \ctx -> ESymbol ctx sym
    keywordValue = do
      reserved ":"
      keyword <- identifier
      pure $ \ctx -> EKeyword ctx keyword
    atomValue = do
      a <- atom
      pure $ \ctx -> EAtom ctx a

binding :: Parser Binding
binding = Single <$> identifier

patternExpr :: Parser Pattern
patternExpr =
  injectContext $
  cases [wildcardPattern, vectorPattern, atomPattern, symbolPattern]
  where
    wildcardPattern = reserved "_" $> \ctx -> PWildcard ctx
    symbolPattern = do
      sym <- binding
      pure $ \ctx -> PSymbol ctx sym
    vectorPattern = do
      subpatterns <- vector $ many patternExpr
      pure $ \ctx -> PVector ctx subpatterns
    atomPattern = do
      atm <- atom
      pure $ \ctx -> PAtom ctx atm

atom :: Parser Atom
atom = cases [unitAtom, integerAtom, stringAtom, booleanAtom]
  where
    unitAtom = do
      reserved "nil"
      pure AUnit
    integerAtom = do
      mnum <- readMaybe <$> many1 digit
      case mnum of
        Nothing  -> parserFail "couldn't decipher integer"
        Just num -> pure $ AInteger num
    stringAtom =
      AString . toText <$> between (char '"') (char '"') (many $ noneOf "\"")
    booleanAtom =
      ABoolean <$> choice [reserved "true" $> True, reserved "false" $> False]
