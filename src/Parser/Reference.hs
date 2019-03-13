module Parser.Reference
  ( variableName
  , moduleName
  , typeName
  , keyword
  )
where

import           Parser.Common
import           Syntax.Reference               ( Keyword(..)
                                                , Module(..)
                                                , Type(..)
                                                , Value(..)
                                                )


variableName :: Parser Value
variableName = cases [qualifiedName, localName]
 where
  qualifiedName = do
    modul <- moduleName
    char '/'
    Remote modul <$> identifier
  localName = Local <$> identifier

moduleName :: Parser Module
moduleName = Module <$> identifier `sepBy1` char '.'

typeName :: Parser Type
typeName = do
  _ <- lookAhead upper
  Type <$> identifier

keyword :: Parser Keyword
keyword = do
  _ <- char ':'
  Keyword <$> variableName
