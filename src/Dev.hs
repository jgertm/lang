module Dev where

import qualified Data.Map.Strict               as Map
import           Data.Text.Prettyprint.Doc
import           Text.Pretty.Simple
import qualified Universum.Unsafe              as Unsafe

import           Classes
import qualified Interpreter
import qualified Interpreter.Types             as Interpreter
import qualified Parser
import qualified Syntax.Atom                   as Atom
import qualified Syntax.Definition             as Definition
import qualified Syntax.Pattern                as Pattern
import qualified Syntax.Reference              as Reference
import qualified Syntax.Term                   as Term
import qualified Type
import qualified Type.Analysis                 as Analysis
import qualified Type.Context                  as Ctx
import qualified Type.Synthesis                as Synthesis


main :: IO ()
main = do
  putStrLn ""
  -- pPrint $ Type.infer =<< parse "(fn [x] (match x ([:nil _] true) ([:cons {_ _}] false)))"
  -- pPrint $ Type.infer =<< parse "(match 1 (0 [:nil 0] ) (1 [:nil 1]))"
  -- pPrint $ Type.inferWith mempty =<< parse "(if true [:foo 1] [:bar nil])"
  -- pPrint $ Type.inferWith mempty =<< parse "(match (if true [:bar 1] [:foo nil]) ([:bar 1] true) ([:foo nil] false))"
  pPrint $ Type.inferWith mempty =<< parse "(match [:foo 1] ([:foo 2] true) ([:foo 1] false))"

eval = fromRight undefined . Interpreter.eval

parse :: Text -> Either _ (Term.Term Empty)
parse = fmap (meta $ const ()) . Parser.parse Parser.term "<dev>"

file :: Text -> Either _ (Definition.Definition Empty)
file = fmap (meta $ const ()) . Parser.parse Parser.file "<dev>"
