module Dev where

import qualified Parser.Abstract       as AST
import qualified Syntax.Abstract       as AST

import           Data.Foldable
import           Data.Functor.Identity
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Text.Parsec

main :: IO ()
main = do
  let file = "examples/fb.lng"
  contents <- TIO.readFile file
  case parse AST.parser file contents of
    Left error -> print error
    Right ast  -> print ast
