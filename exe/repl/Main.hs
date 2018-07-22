module Main where

import qualified Data.Text                as T
import           System.Console.Haskeline

import           Handlers

main = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "> "
      printHandler outputStrLn minput
      evalHandler outputStrLn minput
      loop
