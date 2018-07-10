module Main where

import           AST
import           Parser

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           Text.Parsec

main :: IO ()
main = TIO.readFile "examples/fb.lng" >>= parseTest parseToplevel
