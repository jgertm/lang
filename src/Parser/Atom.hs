{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parser.Atom
  ( expr
  )
where

import           Parser.Common
import qualified Parser.Lexer                  as Lex
import           Syntax.Atom                    ( Atom(..) )


expr :: Parser Atom
expr = cases [unit, integer, string, boolean]
 where
  unit    = reserved "nil" $> Unit
  integer = Integer <$> Lex.integer
  string  = String <$> Lex.string
  boolean = Boolean <$> choice [reserved "true" $> True, reserved "false" $> False]
