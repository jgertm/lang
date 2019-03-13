module Parser.File
  ( file
  )
where

import           Parser.Common
import           Parser.Definition              ( definition )
import           Syntax.Definition              ( Definition(..) )


file :: Parser (Definition Parsing)
file = do
  (Module ctx moduleName []) <- definition
  definitions                <- many definition
  pure $ Module ctx moduleName definitions

