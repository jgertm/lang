module Main where

import           Data.Foldable
import qualified Data.Text                as T
import           System.Console.Haskeline

import           Handlers

main = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> pure ()
        Just ":quit" -> pure ()
        Just "" -> loop
        Just input -> do
          traverse_
            (\h -> outputStrLn $ h input)
            ([printHandler, symbolHandler, evalHandler] :: [String -> String])
          loop
