module Module
  ( Module(..)
  , load
  , run
  -- , native
  -- , empty
  )
where

import qualified Data.Map.Strict               as Map
import           Data.Sequence                  ( (|>) )
import           Data.Text.Prettyprint.Doc

import           Application                    ( Compile )
import qualified Builtins
import           Classes                        ( Empty
                                                , meta
                                                )
import           Error                          ( Error )
import qualified Error
import qualified Interpreter
import           Interpreter.Types              ( Evaluation )
import qualified Interpreter.Types             as Interpreter
import qualified Parser
import           Parser.Common                  ( Parsing )
import           Syntax.Atom                    ( Atom )
import qualified Syntax.Atom                   as Atom
import           Syntax.Definition              ( Definition )
import qualified Syntax.Definition             as Def
import qualified Syntax.Reference              as Ref
import           Syntax.Term                    ( Term )
import qualified Syntax.Term                   as Term
import           Type                           ( Type )
import qualified Type
import qualified Type.Context                  as Ctx
import qualified Type.Expression               as Type
import qualified Type.Monad                    as Type
import qualified Type.Types                    as Type


data Module = Module
  { name        :: Ref.Module
  , imports     :: [Module]
  , definitions :: [Definition Parsing]
  , typedefs    :: Map Ref.Type Type
  , bindings    :: Map Ref.Value Type
  , closures    :: Map Ref.Value Interpreter.Closure
  } deriving (Generic, Show)

instance Pretty Module where
  pretty Module{name, typedefs, bindings} =
    let intro      = "module" <+> pretty name
        types'     = lined $ map (\(name, body) -> hsep [pretty name, "=", pretty body]) $ toPairs typedefs
        longest    = maximum $ map (length . show . pretty) $ keys bindings
        bindings'  = lined $ map (\(name, typ) -> hsep [fill longest $ pretty name, ":", pretty typ]) $ toPairs bindings
        content    = braces $ align $ vsep [types', bindings']
    in parens $ vsep [intro, indent 2 content]
    where lined = mconcat . punctuate hardline

withinScope :: (Monoid d) => (Module -> d) -> Module -> d
withinScope acc modul =
  let direct     = acc modul
      transitive = foldMap acc $ imports modul
  in  direct <> transitive

parse :: FilePath -> Text -> Compile Module
parse path source = do
  ast@(Def.Module _ name definitions) <- Parser.parse Parser.file path source
  pure $ Module
    { name
    , imports     = [native]
    , definitions
    , typedefs    = mempty
    , bindings    = mempty
    , closures    = mempty
    }

typecheck :: Module -> Compile Module
typecheck mod = foldM process mod $ definitions mod
 where
  process _ Def.Module{} = error "[module] submodule typechecking not yet implemented"

  process modul typedef@(Def.Type _ name _ body) = do
    nameVar <- Type.freshExistential
    let varType = Type.ExistentialVariable nameVar
        body'   = Type.fromSyntax (Map.insert name varType $ withinScope typedefs modul) body
        typ     = if body' `Type.contains` varType then Type.Fix nameVar body' else body'
    pure $ modul { typedefs = Map.insert name typ $ typedefs modul }

  process modul (Def.Constant _ name body) = do
    typ <- Type.inferWith (withinScope typedefs modul) (withinScope bindings modul) body
    pure $ modul { bindings = Map.insert name typ $ bindings modul }


interpret :: Module -> Compile Module
interpret mod = foldM process mod $ definitions mod
 where
  process _ Def.Module{} = error "[module] submodule interpretation not yet implemented"

  process modul (Def.Constant _ name body) = do
    let body'   = meta (const ()) body
        closure = Interpreter.Closure body' $ withinScope closures modul
    pure $ modul { closures = Map.insert name closure $ closures modul }

  process modul Def.Type{} = pure modul

load :: FilePath -> Text -> Compile Module
load path source = parse path source >>= typecheck >>= interpret

run :: Module -> Compile Atom
run Module { closures } = do
  let Interpreter.Closure main env =
        fromMaybe (error "[module] no main function found") $ Map.lookup (Ref.Local "main") closures
  case Interpreter.evalWith env (Term.Application () main [Term.Atom () Atom.Unit]) of
    Right (Term.Atom _ atom) -> pure atom
    _                        -> throwError $ Error.Interpretation Error.Unknown


native :: Module
native = Module
  { name        = Ref.Module ["lang", "core", "native"]
  , imports     = mempty
  , definitions = mempty
  , typedefs    = Type.natives
  , bindings    = map Builtins.typ Builtins.builtins
  , closures    = Interpreter.builtins
  }
