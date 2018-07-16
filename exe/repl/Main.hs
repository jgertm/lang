module Main where

import qualified Data.Text                as T
import           System.Console.Haskeline

import           Parser.Abstract
import           Syntax.Abstract
import           Text.Parsec

main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just input -> do
          outputStrLn $
            show $
            parse
              (cases
                 [ show <$> definition
                 , show <$> typeExpr
                 , show <$> valueExpr
                 , show <$> atom
                 ])
              "<repl>" $
            T.pack input
          loop
