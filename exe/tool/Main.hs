module Main where

import           Data.Text.Prettyprint.Doc
import qualified System.Environment            as Env

import           Classes
import qualified Module
import qualified Parser
import qualified Syntax.Term                   as Term


main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    []             -> greet
    ["type", file] -> do
      content <- readFile file
      case Module.signature =<< Parser.parse Parser.file "<input>" content of
        Right sig -> putTextLn $ show $ pretty sig
        Left  err -> do
          putStrLn "ERROR"
          print err
    ["run", file] -> do
      content <- readFile file
      let ast = Parser.parse Parser.file "<input>" content
      guard $ isRight $ Module.signature =<< ast
      case Module.run =<< ast of
        Left  err                -> print err
        Right (Term.Atom _ atom) -> putTextLn $ show $ pretty atom


greet :: IO ()
greet = putTextLn $ unlines
  [" _             ", "| |___ ___ ___ ", "| | .'|   | . |", "|_|__,|_|_|_  |", "          |___|"]

