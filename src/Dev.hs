module Dev where

import           Text.Pretty.Simple

import           Parser

main :: IO ()
main = do
  let path = "examples/fb.lng"
  contents <- readFile path
  case parse file path contents of
    Left e    -> pPrint e
    Right ast -> pPrint ast
