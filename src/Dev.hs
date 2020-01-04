{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Dev where

import qualified Data.Map.Strict               as Map
import           Data.Text.Prettyprint.Doc
import           Text.Pretty.Simple
import qualified Universum.Unsafe              as Unsafe
import qualified System.IO

import qualified Application                   as App
import           Classes
import qualified Error
import qualified Interpreter
import qualified Interpreter.Types             as Interpreter
import qualified Module
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
  System.IO.hSetEncoding System.IO.stdout System.IO.utf8
  putStrLn ""
  let path = "examples/linked-list.lang"
  src <- readFile path
  let Right (modul, result) = App.run $ do
        modul <- Module.load path src
        inspect $ Module.typedefs modul
        result <- Module.run modul
        pure (modul, result)
  print . pretty $ modul
  print . pretty $ result
  pass



eval :: Term.Term phase -> Interpreter.Term
eval = fromRight undefined . Interpreter.eval

infer = Type.infer

parse :: Text -> Either Error.Error (Term Empty)
parse = fmap (meta $ const ()) . Parser.parse Parser.term "<dev>"

file :: Text -> Either Error.Error (Def.Definition Empty)
file = fmap (meta $ const ()) . Parser.parse Parser.file "<dev>"
