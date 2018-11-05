module Main where

import           System.Console.ANSI

import qualified Module
import qualified Syntax

main :: IO ()
main = do
  args <- getArgs
  case args of
    arg : _ -> do
      initial <- Module.initialize arg
      let result = pipeline initial in render result
    _ -> greet

greet = putTextLn $ unlines
  [ " _             "
  , "| |___ ___ ___ "
  , "| | .'|   | . |"
  , "|_|__,|_|_|_  |"
  , "          |___|"
  ]

pipeline = Module.process >=> Module.run

render (Right (Syntax.EAtom _ atom)) = putTextLn $ Syntax.showAtom atom
render (Left  err                  ) = do
  setSGR [SetColor Foreground Vivid Red]
  print err
  setSGR [Reset]
  exitFailure
