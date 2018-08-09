module Dev where

import           Text.Pretty.Simple

import qualified Inference
import qualified Interpreter
import qualified Parser
import qualified Renaming
import qualified Syntax

main :: IO ()
main = do
  let path = "examples/if.lang"
  contents <- readFile path
  pPrint . infer $ parse contents

parse = fromRight undefined . Parser.parse Parser.term "<input>"

rename = fromRight undefined . Renaming.rename

interpret = fromRight undefined . Interpreter.interpretWith mempty

infer = Inference.infer
