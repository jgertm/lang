module Handlers where

import Control.Monad
import Data.Bifunctor
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Pretty.Simple

import Error
import Interpreter
import Parser
import Parser.Abstract

printHandler pr minp =
  case minp of
    Just ":quit" -> return ()
    Just input -> do
      let result =
            parse
              (cases
                 [ pShow <$> definition
                 , pShow <$> typeExpr
                 , pShow <$> valueExpr
                 , pShow <$> atom
                 ])
              "<repl>" $
            T.pack input
      case result of
        Left e -> pr $ show e
        Right (ast :: _) -> pr . T.unpack . TL.toStrict $ ast

evalHandler pr minp =
  case minp of
    Just ":quit" -> return ()
    Just input ->
      pr $
      show $
      runInterpreter M.empty . interpretValue <=< parse valueExpr "<repl>" $
      T.pack input
