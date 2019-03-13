module Parser.Lexer
  ( Parser
  , reserved
  , identifier
  , integer
  , string
  , cases
  , sexp
  , vector
  , record
  , tuple
  , variant
  )
where

import           Text.Parsec                    ( ParsecT
                                                , choice
                                                , letter
                                                , oneOf
                                                , try
                                                )
import           Text.Parsec.Token       hiding ( identifier
                                                , integer
                                                , reserved
                                                )
import qualified Text.Parsec.Token             as P


type Parser a = ParsecT Text () Identity a

lang :: (Monad m) => GenLanguageDef Text () m
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

lexer :: (Monad m) => GenTokenParser Text () m
lexer = makeTokenParser lang

ws :: Parser ()
ws = whiteSpace lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer

identifier :: Parser Text
identifier = toText <$> P.identifier lexer

integer :: Parser Integer
integer = P.integer lexer

string :: Parser Text
string = toText <$> P.stringLiteral lexer

trimr :: Parser a -> Parser a
trimr p = p <* ws

cases :: [Parser a] -> Parser a
cases = trimr . choice . map try

sexp, vector, record, tuple, variant :: Parser a -> Parser a
sexp = parens lexer

vector = brackets lexer

record = braces lexer

tuple = braces lexer

variant = brackets lexer
