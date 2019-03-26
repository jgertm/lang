module Parser
  ( parse
  , file
  , definition
  , term
  , typ
  )
where

import qualified Text.Parsec                   as Parsec

import           Error                          ( Error(Parsing) )
import           Parser.Common
import           Parser.Definition              ( definition )
import           Parser.File                    ( file )
import qualified Parser.Term                   as Term
import qualified Parser.Type                   as Type
import           Syntax.Term                    ( Term )
import           Syntax.Type                    ( Type )


type MonadParse m = (MonadError Error m)
type Parse a = forall m. (MonadParse m) => m a

parse :: Parser a -> FilePath -> Text -> Parse a
parse parser sourcePath input = liftEither $ first Parsing $ Parsec.parse parser sourcePath input

term :: Parser (Term Parsing)
term = Term.expr

typ :: Parser (Type Parsing)
typ = Type.expr
