module Module where

import qualified Data.Map.Strict               as Map
import           Data.Text.Prettyprint.Doc

import           Classes
import           Error
import qualified Interpreter
import qualified Interpreter.Types             as Interpreter
import qualified Renaming
import qualified Syntax.Definition             as Def
import qualified Syntax.Reference              as Syntax
import qualified Syntax.Term                   as Term
import qualified Type


type Term = Term.Term Empty

data Signature = Signature Syntax.Module (Map Syntax.Value Type.Type)

instance Pretty Signature where
  pretty (Signature name bindings) =
    let max = maximum $ map (length . show . pretty) $ keys bindings
        meta = "module" <+> pretty name
        content = braces $ align $ vsep $ map (\(binding, typ) -> hsep [fill max $ pretty binding, ":", pretty typ]) $ toPairs bindings
    in parens $ vsep [meta, indent 2 content]

types = Type.builtins

typecheck
  :: Map Syntax.Value Type.Type
  -> Map Syntax.Value Type.Type
  -> Def.Definition phase
  -> Either Error (Map Syntax.Value Type.Type)
typecheck globals env (Def.Constant _ name body) = do
  typ <- Type.inferWith (Map.union env globals) body
  pure $ Map.singleton name typ
typecheck globals env (Def.Module _ name definitions) = foldM
  (\acc def -> do
    types <- typecheck globals acc def
    let acc' = Map.union types acc
    pure acc'
  )
  env
  definitions

signature :: Def.Definition phase -> Either Error Signature
signature def@(Def.Module _ name _) = Signature name <$> typecheck types mempty def

functions = Interpreter.builtins

load
  :: Map Syntax.Value Interpreter.Closure
  -> Def.Definition phase
  -> Map Syntax.Value Interpreter.Closure
load env (Def.Constant _ name body) =
  Map.insert name (Interpreter.Closure (meta (const ()) body) env) env
load env (Def.Module _ _ definitions) = foldl' load env definitions

run :: Def.Definition phase -> Either Error Term
run modul@Def.Module{} = do
  Interpreter.Closure main env <-
    liftMaybe (Module NoMainFunction) $ Map.lookup (Syntax.Local "main") $ load functions modul
  meta (const ()) <$> Interpreter.evalWith (Map.union env functions) main




