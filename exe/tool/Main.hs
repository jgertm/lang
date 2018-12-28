module Main where

import           System.Console.ANSI
import qualified System.Environment            as Env


main :: IO ()
main = greet

greet = putTextLn $ unlines
  [" _             ", "| |___ ___ ___ ", "| | .'|   | . |", "|_|__,|_|_|_  |", "          |___|"]
