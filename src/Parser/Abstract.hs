module Parser.Abstract
  ( Parser
  , name
  , file
  , definition
  , typeExpr
  , valueExpr
  , patternExpr
  , atom
  , cases
  ) where

import           Control.Monad
import           Data.Functor
import           Data.Text         (Text)
import qualified Data.Text         as T
import           Text.Parsec
import           Text.Parsec.Token hiding (identifier, reserved)
import qualified Text.Parsec.Token as P

import           Syntax.Abstract

type Parser a = Parsec Text () a

parser :: Parser Definition
parser = file

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

name = VSymbol <$> identifier

file :: Parser Definition
file = do
  (DModule moduleName []) <- definition
  definitions <- many definition
  pure $ DModule moduleName definitions

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
        DConstant <$> identifier <*> valueExpr
    functionDefinition =
      sexp $ do
        reserved "defn"
        functionName <- identifier
        functionArguments <- vector $ many identifier
        functionBody <- many valueExpr
        pure $
          DFunction functionName functionArguments $
          case functionBody of
            [form] -> form
            forms  -> VSequence forms

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

valueExpr :: Parser ValueExpr
valueExpr =
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
        body <- valueExpr
        pure $ foldr VLambda body args
    ifValue =
      sexp $ do
        reserved "if"
        VIf <$> valueExpr <*> valueExpr <*> valueExpr
    matchValue =
      sexp $ do
        reserved "match"
        VMatch <$> valueExpr <*> many1 pattrn
      where
        pattrn = sexp $ (,) <$> patternExpr <*> valueExpr
    sequenceValue =
      sexp $ do
        reserved "do"
        VSequence <$> many1 valueExpr
    letValue =
      sexp $ do
        reserved "let"
        bindings <- vector $ many binding
        body <- valueExpr
        pure $ foldr (\(name, body) inner -> VLet name body inner) body bindings
      where
        binding = vector $ (,) <$> identifier <*> valueExpr
    applicationValue =
      sexp $ do
        function <- valueExpr
        args <- many valueExpr
        pure $ foldl VApplication function args
    recordValue = record $ VRecord <$> many1 row
      where
        row = (,) <$> identifier <*> valueExpr
    vectorValue = VVector <$> vector (many valueExpr)
    symbolValue = VSymbol <$> identifier
    atomValue = VAtom <$> atom

patternExpr :: Parser PatternExpr
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
      reserved "()"
      pure AUnit
    integerAtom = AInteger . read <$> many1 digit
    stringAtom =
      AString . T.pack <$> between (char '"') (char '"') (many $ noneOf "\"")
    keywordAtom = AKeyword <$> (char ':' *> identifier)
    booleanAtom =
      ABoolean <$> choice [reserved "true" $> True, reserved "false" $> False]
