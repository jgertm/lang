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
import qualified Syntax.Definition             as Def
import qualified Syntax.Pattern                as Pattern
import qualified Syntax.Reference              as Reference
import qualified Syntax.Term                   as Term
import qualified Type
import qualified Type.Analysis                 as Analysis
import qualified Type.Context                  as Ctx
import qualified Type.Expression               as Expression
import qualified Type.Match                    as Match
import qualified Type.Monad                    as Monad
import qualified Type.Synthesis                as Synthesis
import           Type.Types


main :: IO ()
main = do
  Right (Def.Module _ _ [typedef, _]) <- file <$> readFile "examples/linked-list.lang"
  putStrLn ""
  let Right nil  = parse "[:nil nil]"
      Right cons = parse "[:cons {2 [:nil nil]}]"
  let result = Monad.run $ do
        (list, ctx) <- Expression.fromDefinition Expression.builtins typedef
        Analysis.check ctx cons (list, Principal)
  pPrint result


eval :: Term.Term phase -> Interpreter.Term
eval = fromRight undefined . Interpreter.eval

parse :: Text -> Either Error.Error (Term.Term Empty)
parse = fmap (meta $ const ()) . Parser.parse Parser.term "<dev>"

file :: Text -> Either Error.Error (Def.Definition Empty)
file = fmap (meta $ const ()) . Parser.parse Parser.file "<dev>"
