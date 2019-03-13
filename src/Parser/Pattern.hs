module Parser.Pattern
  ( expr
  )
where

import qualified Data.Map.Strict               as Map

import qualified Parser.Atom                   as Atom
import           Parser.Common
import           Parser.Reference               ( keyword
                                                , variableName
                                                )
import           Syntax.Pattern                 ( Pattern(..) )


expr :: Parser (Pattern Parsing)
expr = injectContext $ cases
  [ wildcardPattern
  , vectorPattern
  , tuplePattern
  , recordPattern
  , variantPattern
  , atomPattern
  , symbolPattern
  ]
 where
  wildcardPattern = reserved "_" $> \ctx -> Wildcard ctx
  symbolPattern   = do
    sym <- variableName
    pure $ \ctx -> Symbol ctx sym
  vectorPattern = vector $ do
    subpatterns <- many expr
    pure $ \ctx -> Vector ctx subpatterns
  tuplePattern = tuple $ do
    subpatterns <- Map.fromList . zip [1 ..] <$> many1 expr
    pure $ \ctx -> Tuple ctx subpatterns
  recordPattern = record $ do
    subpatterns <- Map.fromList <$> many row
    pure $ \ctx -> Record ctx subpatterns
    where row = (,) <$> keyword <*> expr
  variantPattern = variant $ do
    tag        <- keyword
    subpattern <- expr
    pure $ \ctx -> Variant ctx tag subpattern
  atomPattern = do
    atm <- Atom.expr
    pure $ \ctx -> Atom ctx atm
