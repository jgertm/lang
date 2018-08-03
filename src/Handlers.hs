module Handlers where

import qualified Data.Map.Strict    as M
import           Text.Pretty.Simple

import           Inference
import           Interpreter
import           Parser
import           Renaming
import           Syntax

printHandler, evalHandler, symbolHandler, labelHandler, constraintHandler ::
     String -> String
printHandler input = do
  let result =
        parse
          (cases
             -- pShow <$> definition
             -- , pShow <$> typeExpr
             [expr, EAtom [] <$> atom])
          "<repl>" $
        toText input
  case result of
    Left e    -> show e
    Right ast -> pretty . evaluatingState 0 . label $ ast

evalHandler input =
  show $
  runInterpreter M.empty . interpretValue <=< rename <=< parse expr "<repl>" $
  toText input

symbolHandler input =
  let east = parse expr "<repl>" $ toText input
   in case east of
        Left _    -> "error in symbol accumulation"
        Right ast -> show . evalWriter . descendM pub $ ast
  where
    pub :: (MonadWriter [Name] m) => ExprA ann -> m (ExprA ann)
    pub =
      \case
        sym@(ESymbol _ name) -> tell [name] $> sym
        e -> pure e

labelHandler input =
  let east = parse expr "<repl>" $ toText input
   in case east of
        Left _    -> "error in symbol accumulation"
        Right ast -> show $ evaluatingState 0 . label $ ast

constraintHandler input =
  let east = parse expr "<repl>" $ toText input
   in case east of
        Left _ -> "error in symbol accumulation"
        Right ast ->
          show . evalWriter . descendM constrain . evaluatingState 0 . label $
          ast

pretty :: (Show a) => a -> String
pretty = toString . pShow
