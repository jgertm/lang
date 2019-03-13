module Parser.Type
  ( expr
  )
where

import           Parser.Common
import           Parser.Reference               ( keyword
                                                , typeName
                                                )
import           Syntax.Type                    ( Type(..) )


expr :: Parser (Type Parsing)
expr = injectContext
  $ cases [namedType, applicationType, tupleType, recordType, variantType, functionType]
 where
  namedType = do
    name <- typeName
    pure $ \ctx -> Named ctx name
  applicationType = sexp $ do
    operator <- typeName
    params   <- many1 expr
    pure $ \ctx -> Application ctx operator params
  tupleType = tuple $ do
    elements <- many1 expr
    pure $ \ctx -> Tuple ctx elements
  recordType = record $ do
    rows <- record (many1 row)
    pure $ \ctx -> Record ctx rows
    where row = (,) <$> keyword <*> expr
  variantType = sexp $ do
    reserved "|"
    variants <- many1 $ variant $ (,) <$> keyword <*> expr
    pure $ \ctx -> Variant ctx variants
  functionType = sexp $ do
    reserved "fn"
    domains <- many1 expr
    pure $ \ctx -> Function ctx domains
