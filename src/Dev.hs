module Dev where

import qualified Data.Map.Strict               as Map
import           Data.Text.Prettyprint.Doc
import           Text.Pretty.Simple
import qualified Universum.Unsafe              as Unsafe

import           Classes
import qualified Error
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
  -- pPrint $ Type.infer =<< parse "(match 1 (0 {1 2} ) (1 {3 4}))"
  -- pPrint $ Type.inferWith mempty =<< parse "(if true [:foo 1] [:bar nil])"
  -- pPrint $ Type.inferWith mempty =<< parse "(match (if true [:bar 1] [:foo nil]) ([:bar 1] true) ([:foo nil] false))"
  -- pPrint $ Type.inferWith mempty =<< parse "(match [:foo 1] ([:foo 2] true) ([:foo 1] false))"
  -- pPrint $ Type.inferWith mempty =<< parse "(fn [x] (match x ([:foo 2] true) ([:foo 1] false)))"
  -- pPrint $ Type.inferWith mempty =<< parse "(match 1 (2 [:foo 1]) (n [:bar n]))"
  -- pPrint $ Type.inferWith mempty =<< parse "{:foo 1 :bar true}"
  -- pPrint $ Type.inferWith mempty =<< parse "(fn [x] (match x ({true _} 1)))"
  -- pPrint $ Type.inferWith mempty =<< parse "(match {true true} ({true _} 1))"
  -- pPrint $ Type.inferWith mempty =<< parse "(fn [x] (match x ([:foo nil] 1) ([:bar true] 2)))"
  -- pPrint $ Type.inferWith mempty =<< parse
  --   "(match {:foo 1 :bar true} ({:foo 2} 1)  ({:foo 1 :bar true} 3))"
  pPrint $ Type.inferWith mempty =<< parse "(fn [x] (match x ({:foo 2} 1) ({:foo 1 :bar true} 2)))"


eval :: Term.Term phase -> Interpreter.Term
eval = fromRight undefined . Interpreter.eval

parse :: Text -> Either Error.Error (Term.Term Empty)
parse = fmap (meta $ const ()) . Parser.parse Parser.term "<dev>"

file :: Text -> Either Error.Error (Definition.Definition Empty)
file = fmap (meta $ const ()) . Parser.parse Parser.file "<dev>"
