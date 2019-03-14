{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Dev where

import qualified Data.Map.Strict               as Map
import           Data.Text.Prettyprint.Doc
import           Text.Pretty.Simple
import qualified Universum.Unsafe              as Unsafe

import qualified Application                   as App
import           Classes
import qualified Error
import qualified Interpreter
import qualified Interpreter.Types             as Interpreter
import qualified Parser
import qualified Syntax.Atom                   as Atom
import qualified Syntax.Definition             as Def
import qualified Syntax.Pattern                as Pattern
import qualified Syntax.Reference              as Reference
import           Syntax.Term                    ( Term )
import qualified Syntax.Term                   as Term
import qualified Type
import qualified Type.Analysis                 as Analysis
import qualified Type.Context                  as Ctx
import qualified Type.Expression               as Type
import qualified Type.Match                    as Match
import qualified Type.Monad                    as Monad
import qualified Type.Synthesis                as Synthesis
import           Type.Types                     ( Principality(..) )


main :: IO ()
main = do
  putStrLn ""
  Right ast@(Def.Module _ _ (typedef : empty : nth)) <- file
    <$> readFile "examples/linked-list.lang"
  inspect typedef
  -- pPrint nth
  putStrLn ""
  let Right nil   = parse "[:list/nil nil]"
      Right cons  = parse "[:list/cons {2 [:list/nil nil]}]"
      Right ccons = parse "[:list/cons {1 [:list/cons {2 [:list/nil nil]}]}]"
  let result =
        App.run
          $ App.App
          $ do
              (list, ctx) <- Type.fromDefinition Type.natives typedef
              -- inspect list
              Analysis.check ctx ccons (list, Principal)
  pPrint result
  pass



eval :: Term.Term phase -> Interpreter.Term
eval = fromRight undefined . Interpreter.eval

parse :: Text -> Either Error.Error (Term Empty)
parse = fmap (meta $ const ()) . Parser.parse Parser.term "<dev>"

file :: Text -> Either Error.Error (Def.Definition Empty)
file = fmap (meta $ const ()) . Parser.parse Parser.file "<dev>"
