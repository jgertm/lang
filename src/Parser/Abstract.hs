module Parser.Abstract
  ( Parser
  , name
  , file
  , definition
  , typeExpr
  , expr
  , patternExpr
  , atom
  , cases
  ) where

import           Data.Functor
import           Data.Functor.Identity
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Text.Parsec
import           Text.Parsec.Token     hiding (identifier, reserved)
import qualified Text.Parsec.Token     as P

import           Syntax.Abstract

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

cases = trimr . choice . fmap try

sexp = parens lexer

vector = brackets lexer

record = braces lexer

identifier = T.pack <$> P.identifier lexer

reserved = P.reserved lexer

name =
  withExtent $ do
    n <- identifier
    pure $ \meta -> ESymbol meta n

file :: Parser Definition
file = do
  ws
  (DModule moduleName []) <- definition
  definitions <- many definition
  pure $ DModule moduleName definitions

withExtent :: Parser (Meta -> ExprA Meta) -> Parser (ExprA Meta)
withExtent p = do
  start <- getPosition
  result <- p
  end <- getPosition
  let meta = Meta {extent = (start, end)}
  pure $ result meta

definition :: Parser Definition
definition =
  cases
    [moduleDefinition, typeDefinition, constantDefinition, functionDefinition]
  where
    moduleDefinition =
      sexp $ do
        reserved "defmodule"
        DModule <$> identifier <*> many definition
    typeDefinition =
      sexp $ do
        reserved "deftype"
        typeName <- identifier
        typeBody <- typeExpr
        DType <$> identifier <*> typeExpr
    constantDefinition =
      sexp $ do
        reserved "def"
        DConstant <$> identifier <*> expr
    functionDefinition = do
      start <- getPosition
      sexp $ do
        reserved "defn"
        functionName <- identifier
        functionArguments <- vector $ many identifier
        functionBody <- many expr
        end <- getPosition
        let meta = Meta {extent = (start, end)}
        pure $
          DFunction functionName functionArguments $
          case functionBody of
            [form] -> form
            forms  -> ESequence meta forms

typeExpr :: Parser TypeExpr
typeExpr = cases [productType, sumType, recordType, tagType, functionType]
  where
    productType =
      sexp $ do
        reserved "product"
        TProduct <$> many1 typeExpr
    sumType =
      sexp $ do
        reserved "sum"
        TSum <$> many1 typeExpr
    recordType =
      sexp $ do
        reserved "record"
        TRecord <$> record (many1 row)
      where
        row = (,) <$> identifier <*> typeExpr
    tagType =
      sexp $ do
        reserved "tag"
        TTag <$> identifier <*> typeExpr
    functionType =
      sexp $ do
        reserved "fn"
        TFunction <$> many1 typeExpr

expr :: Parser (ExprA Meta)
expr =
  withExtent $
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
    integerAtom = AInteger . read <$> many1 digit
    stringAtom =
      AString . T.pack <$> between (char '"') (char '"') (many $ noneOf "\"")
    keywordAtom = AKeyword <$> (char ':' *> identifier)
    booleanAtom =
      ABoolean <$> choice [reserved "true" $> True, reserved "false" $> False]
