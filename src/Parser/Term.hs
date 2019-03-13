module Parser.Term
  ( expr
  )
where

import qualified Data.Map.Strict               as Map

import qualified Parser.Atom                   as Atom
import           Parser.Common
import qualified Parser.Pattern                as Pattern
import           Parser.Reference               ( keyword
                                                , variableName
                                                )
import           Syntax.Term                    ( Branch(..)
                                                , Term(..)
                                                )


expr :: Parser (Term Parsing)
expr = injectContext $ cases
  [ lambdaTerm
  , ifTerm
  , matchTerm
  , sequenceTerm
  , letTerm
  , fixTerm
  , applicationTerm
  , tupleTerm
  , recordTerm
  , variantTerm
  , vectorTerm
  , atomTerm
  , symbolTerm
  ]
 where
  lambdaTerm = sexp $ do
    reserved "fn"
    args <- vector $ many variableName
    body <- many expr
    pure $ \ctx ->
      let body' = case body of
            [form] -> form
            forms  -> Sequence ctx forms
      in  foldr' (Lambda ctx) body' args
  ifTerm = sexp $ do
    reserved "if"
    test <- expr
    thn  <- expr
    els  <- expr
    pure $ \ctx -> If ctx test thn els
  matchTerm = sexp $ do
    reserved "match"
    body     <- expr
    branches <- many branch
    pure $ \ctx -> Match ctx body branches
   where
    branch = sexp $ do
      patterns <- alternatives <|> single
      body     <- expr
      pure $ Branch {patterns , body }
    single       = one <$> Pattern.expr
    alternatives = sexp $ do
      reserved "|"
      many1 Pattern.expr
  sequenceTerm = sexp $ do
    reserved "do"
    steps <- many1 expr
    pure $ \ctx -> Sequence ctx steps
  letTerm = sexp $ do
    reserved "let"
    bindings <- vector $ many association
    body     <- expr
    pure $ \ctx -> Let ctx bindings body
    where association = vector $ (,) <$> variableName <*> expr
  applicationTerm = sexp $ do
    function <- expr
    args     <- many expr
    pure $ \ctx -> Application ctx function args
  tupleTerm = tuple $ do
    fields <- Map.fromList . zip [1 ..] <$> many1 expr
    pure $ \ctx -> Tuple ctx fields
  recordTerm = record $ do
    rows <- Map.fromList <$> many row
    pure $ \ctx -> Record ctx rows
    where row = (,) <$> keyword <*> expr
  variantTerm = variant $ do
    tag  <- keyword
    body <- expr
    pure $ \ctx -> Variant ctx tag body
  vectorTerm = vector $ do
    elements <- many expr
    pure $ \ctx -> Vector ctx elements
  symbolTerm = do
    sym <- variableName
    pure $ \ctx -> Symbol ctx sym
  atomTerm = do
    a <- Atom.expr
    pure $ \ctx -> Atom ctx a
  fixTerm = sexp $ do
    reserved "recur"
    var  <- variableName
    body <- expr
    pure $ \ctx -> Fix ctx var body
