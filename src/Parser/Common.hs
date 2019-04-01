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
import qualified Syntax.Common                 as Syntax


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

withExtent :: Parser (Syntax.Extent -> ast) -> Parser ast
withExtent p = do
  extent <- option Syntax.Closed (reserved "#+" $> Syntax.Open)
  result <- p
  pure $ result extent
