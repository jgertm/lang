module Main where

import           System.Console.Haskeline

import           Handlers

main = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> pass
        Just ":quit" -> pass
        Just "" -> loop
        Just input -> do
          traverse_
            (\h -> outputStrLn $ h input)
            ([ printHandler
             , symbolHandler
             , evalHandler
             , labelHandler
             , constraintHandler
             ] :: [String -> String])
          loop
