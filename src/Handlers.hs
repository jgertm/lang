module Handlers where

import           Control.Monad
import           Control.Monad.Writer
import           Data.Bifunctor
import           Data.Functor
import           Data.Functor.Identity
import qualified Data.Map.Strict       as M
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as TL
import           Text.Pretty.Simple

import           Error
import           Interpreter
import           Parser
import           Parser.Abstract
import           Renaming
import           Syntax.Abstract

printHandler, evalHandler, symbolHandler :: String -> String
printHandler input = do
  let result =
        parse
          (cases
             [ pShow <$> definition
             , pShow <$> typeExpr
             , pShow <$> expr
             , pShow <$> atom
             ])
          "<repl>" $
        T.pack input
  case result of
    Left e    -> show e
    Right ast -> T.unpack . TL.toStrict $ ast

evalHandler input =
  show $
  runInterpreter M.empty . interpretValue <=< rename <=< parse expr "<repl>" $
  T.pack input

symbolHandler input =
  let east = parse expr "<repl>" $ T.pack input
   in case east of
        Left _    -> "error in symbol accumulation"
        Right ast -> show $ snd . runWriter . descendM pub $ ast
  where
    pub :: (MonadWriter [Name] m) => ExprA ann -> m (ExprA ann)
    pub =
      \case
        sym@(ESymbol _ name) -> tell [name] $> sym
        e -> pure e
