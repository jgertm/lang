module Dev where

import qualified Data.Text.IO       as TIO
import           Text.Pretty.Simple

import           Parser
import           Parser.Abstract

main :: IO ()
main = do
  let path = "examples/fb.lng"
  contents <- TIO.readFile path
  case parse file path contents of
    Left e    -> pPrint e
    Right ast -> pPrint ast
