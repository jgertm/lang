module Parser where

import           Data.Bifunctor
import           Data.Text       (Text)
import qualified Text.Parsec     as Parsec

import           Error
import           Parser.Abstract

parse :: Parser a -> String -> Text -> Either Error a
parse parser source input = first Parsing $ Parsec.parse parser source input
