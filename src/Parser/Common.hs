module Parser.Common
  ( module Parser.Common
  , module Parser.Lexer
  , module Text.Parsec
  )
where

import           Text.Parsec             hiding ( many
                                                , parse
                                                , string
                                                , (<|>)
                                                )

import           Classes                        ( Context
                                                , Extra
                                                )
import           Parser.Lexer


data Parsing

type instance Context Parsing = (SourcePos, SourcePos)
type instance Extra Parsing = ()

injectContext :: Parser (Context Parsing -> ast) -> Parser ast
injectContext p = do
  start  <- getPosition
  result <- p
  end    <- getPosition
  let extent = (start, end)
  pure $ result extent
