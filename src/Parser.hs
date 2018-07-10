module Parser where

import           Control.Monad
import           Data.Maybe
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Text.Parsec

import           AST

parseToplevel :: Parsec Text () Expr
parseToplevel = do
  Module n subs <- parseModule
  guard $ null subs
  spaces
  subs' <- parseExprs
  -- eof
  pure $ Module n subs'

parseExpr :: Parsec Text () Expr
parseExpr =
  choice $
  fmap
    try
    [ parseModule
    , parseAnnotation
    , parseFunction
    , parseApplication
    , parseMatch
    , parseVector
    , parseSymbol
    , parseString
    , parseInteger
    , parseKeyword
    , parseUnit
    ]

parseExprs = sepEndBy parseExpr spaces

sexp = between (char '(') (char ')')

vector = between (char '[') (char ']')

identifier = do
  ns <- optionMaybe $ try (dottedNames <* char '/')
  n <- name
  pure $ Identifier ns n

lexeme = do
  first <- choice [letter, oneOf ":!@#$%^&*-_=+|<>?"]
  rest <- many $ choice [letter, digit, oneOf ":!@#$%^&*-_=+|<>?"]
  pure $ T.cons first $ T.pack rest

parseType = choice [primitiveType, constructedType]
  where
    primitiveType = Type <$> identifier <*> pure []
    constructedType =
      sexp $ do
        con <- identifier
        spaces
        args <- parseType `sepBy` spaces
        pure $ Type con args

name = Name <$> lexeme

dottedNames = name `sepBy1` char '.'

parseModule =
  sexp $ do
    string "defmodule"
    spaces
    n <- dottedNames
    spaces
    subs <- parseExprs
    pure $ Module n subs

parseAnnotation =
  sexp $ do
    string "ann"
    spaces
    i <- name
    spaces
    t <- parseType
    pure $ Annotation i t

parseFunction =
  sexp $ do
    string "defn"
    spaces
    n <- name
    spaces
    args <- vector $ sepEndBy name spaces
    spaces
    body <- optionMaybe parseExprs
    pure $ Function n args (fromMaybe [] body)

parseApplication =
  sexp $ do
    function <- identifier
    spaces
    args <- parseExprs
    pure $ Application function args

parseMatch =
  sexp $ do
    string "match"
    spaces
    prototype <- parseExpr
    spaces
    matches <- sepEndBy1 match spaces
    pure $ Match prototype matches
  where match =
          sexp $ do
            pat <- parseExpr
            spaces
            body <- parseExpr
            pure $ (,) pat body

parseVector = Vector <$> vector parseExprs

parseSymbol = Symbol <$> identifier

parseString = String <$> between (char '"') (char '"') (T.pack <$> many (noneOf "\""))

parseInteger = Integer . read <$> many1 digit

parseKeyword = Keyword <$> (char ':' *> name)

parseUnit = do
  string "()"
  pure Unit

