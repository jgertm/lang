{-# LANGUAGE UndecidableInstances #-}

module Module where

import qualified Data.Map.Strict               as Map

import           Classes
import qualified Error
import qualified Inference
import qualified Interpreter
import qualified Parser
import qualified Renaming
import qualified Syntax
import qualified Builtins

type Source = Text

data Empty

type family AST phase

type instance AST Empty = ()

data Module = Module
  { path    :: FilePath
  , source  :: Source
  , symbols :: Env
  } deriving (Generic)

deriving instance Show Module

type Env = Map Syntax.Binding Inference.Definition

initialize path = do
  source <- readFile path
  pure Module
    { path
    , source
    , symbols = Map.mapWithKey
      (\k v ->
        Syntax.DConstant (Syntax.typ v) k (Syntax.ENative (Syntax.typ v) v [])
      )
      Builtins.builtins
    }

process modul@Module { path, source } = do
  (Syntax.DModule _ moduleName definitions) <- liftEither
    $ Parser.parse Parser.file path source
  foldM go modul definitions
 where
  go modul@Module { symbols } def@(Syntax.DConstant _ name body) = do
    inferredDefinition <-
      Inference.inferDefinitionWith (map Syntax.context symbols)
      <=< Renaming.renameWith (map (const 0) symbols)
      $   def
    let extendedSymbols = Map.insert name inferredDefinition symbols
    pure $ modul { symbols = extendedSymbols }

lookup sym mod = case Map.lookup sym mod of
  Nothing -> Left $ Error.Module Error.NoMainFunction
  Just t  -> Right t

run Module { symbols } = do
  let env =
        Map.mapMaybe
            (\case
              (Syntax.DConstant _ _ body) -> Just $ meta (const ()) body
              _                           -> Nothing
            )
          $ symbols
  main <- lookup (Syntax.Single "main") env
  Interpreter.interpretWith env main
