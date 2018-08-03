module Parser
  ( parse
  , Parser
  , name
  , file
  , definition
  , typeExpr
  , expr
  , patternExpr
  , atom
  , cases
  ) where

import           Text.Parsec       hiding (many, parse)
import qualified Text.Parsec       as P
import           Text.Parsec.Token hiding (identifier, reserved)
import qualified Text.Parsec.Token as P

import           Annotation
import           Error
import           Syntax

parse :: Parser a -> String -> Text -> Either Error a
parse parser source input = first Parsing $ P.parse parser source input

type Parser a = ParsecT Text () Identity a

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

lexer = makeTokenParser lang

ws = whiteSpace lexer

trimr p = p <* ws

cases = trimr . choice . fmap P.try

sexp = parens lexer

vector = brackets lexer

record = braces lexer

identifier = toText <$> P.identifier lexer

reserved = P.reserved lexer

injectMetadata :: Parser ([Meta] -> ast Meta) -> Parser (ast Meta)
injectMetadata p = do
  start <- getPosition
  result <- p
  end <- getPosition
  let extent = Extent start end
  pure $ result [extent]

name =
  injectMetadata $ do
    n <- identifier
    pure $ \meta -> ESymbol meta n

file :: Parser (Definition Meta)
file = do
  ws
  (DModule meta moduleName []) <- definition
  definitions <- many definition
  pure $ DModule meta moduleName definitions

definition :: Parser (Definition Meta)
definition =
  injectMetadata $
  cases
    [ moduleDefinition -- typeDefinition,
    , constantDefinition
    , functionDefinition
    ]
  where
    moduleDefinition =
      sexp $ do
        reserved "defmodule"
        name <- identifier
        forms <- many definition
        pure $ \meta -> DModule meta name forms
    typeDefinition =
      sexp $ do
        reserved "deftype"
        name <- identifier
        structure <- typeExpr
        pure $ \meta -> DType meta name structure
    constantDefinition =
      sexp $ do
        reserved "def"
        name <- identifier
        value <- expr
        pure $ \meta -> DConstant meta name value
    functionDefinition =
      sexp $ do
        reserved "defn"
        name <- identifier
        args <- vector $ many identifier
        start <- getPosition
        body <- many expr
        end <- getPosition
        let meta = Extent start end
        pure $ \meta ->
          DFunction meta name args $
          case body of
            [form] -> form
            forms  -> ESequence meta forms

typeExpr :: Parser (TypeExprA Meta)
typeExpr =
  injectMetadata $
  cases [productType, sumType, recordType, tagType, functionType]
  where
    productType =
      sexp $ do
        reserved "product"
        factors <- many1 typeExpr
        pure $ \meta -> TProduct meta factors
    sumType =
      sexp $ do
        reserved "sum"
        summands <- many1 typeExpr
        pure $ \meta -> TSum meta summands
    recordType =
      sexp $ do
        reserved "record"
        rows <- record (many1 row)
        pure $ \meta -> TRecord meta rows
      where
        row = (,) <$> identifier <*> typeExpr
    tagType =
      sexp $ do
        reserved "tag"
        name <- identifier
        inner <- typeExpr
        pure $ \meta -> TTag meta name inner
    functionType =
      sexp $ do
        reserved "fn"
        domains <- many1 typeExpr
        pure $ \meta -> TFunction meta domains

expr :: Parser (ExprA Meta)
expr =
  injectMetadata $
  cases
    [ lambdaValue
    , ifValue
    , matchValue
    , sequenceValue
    , letValue
    , applicationValue
    , recordValue
    , vectorValue
    , atomValue
    , symbolValue
    ]
  where
    lambdaValue =
      sexp $ do
        reserved "fn"
        args <- vector $ many identifier
        body <- expr
        pure $ \meta -> foldr (ELambda meta) body args
    ifValue =
      sexp $ do
        reserved "if"
        test <- expr
        thn <- expr
        els <- expr
        pure $ \meta -> EIf meta test thn els
    matchValue =
      sexp $ do
        reserved "match"
        body <- expr
        patterns <- many1 pattrn
        pure $ \meta -> EMatch meta body patterns
      where
        pattrn = sexp $ (,) <$> patternExpr <*> expr
    sequenceValue =
      sexp $ do
        reserved "do"
        steps <- many1 expr
        pure $ \meta -> ESequence meta steps
    letValue =
      sexp $ do
        reserved "let"
        bindings <- vector $ many binding
        body <- expr
        pure $ \meta ->
          foldr (\(name, body) inner -> ELet meta name body inner) body bindings
      where
        binding = vector $ (,) <$> identifier <*> expr
    applicationValue =
      sexp $ do
        function <- expr
        args <- many expr
        pure $ \meta -> foldl (EApplication meta) function args
    recordValue =
      record $ do
        rows <- many1 row
        pure $ \meta -> ERecord meta rows
      where
        row = (,) <$> identifier <*> expr
    vectorValue =
      vector $ do
        elements <- many expr
        pure $ \meta -> EVector meta elements
    symbolValue = do
      sym <- identifier
      pure $ \meta -> ESymbol meta sym
    atomValue = do
      a <- atom
      pure $ \meta -> EAtom meta a

patternExpr :: Parser Pattern
patternExpr = cases [wildcardPattern, vectorPattern, atomPattern, symbolPattern]
  where
    symbolPattern = PSymbol <$> identifier
    vectorPattern = PVector <$> vector (many patternExpr)
    atomPattern = PAtom <$> atom
    wildcardPattern = PWildcard <$ reserved "_"

atom :: Parser Atom
atom = cases [unitAtom, integerAtom, stringAtom, keywordAtom, booleanAtom]
  where
    unitAtom = do
      reserved "nil"
      pure AUnit
    integerAtom = do
      number <- readMaybe <$> many1 digit
      case number of
        Nothing     -> parserFail "couldn't decipher integer"
        Just number -> pure $ AInteger number
    stringAtom =
      AString . toText <$> between (char '"') (char '"') (many $ noneOf "\"")
    keywordAtom = AKeyword <$> (char ':' *> identifier)
    booleanAtom =
      ABoolean <$> choice [reserved "true" $> True, reserved "false" $> False]
