module Module
  ( load
  , run
  , native
  , empty
  , Module(..)
  )
where

import qualified Data.Map.Strict               as Map
import           Data.Sequence                  ( (|>) )
import           Data.Text.Prettyprint.Doc

import qualified Builtins
import           Classes                        ( Empty
                                                , meta
                                                )
import           Error                          ( Error )
import qualified Interpreter
import qualified Interpreter.Types             as Interpreter
import qualified Syntax.Definition             as Def
import qualified Syntax.Reference              as Syntax
import qualified Syntax.Term                   as Term
import qualified Type


type Term = Term.Term Empty
data Module = Module
  { name      :: Ref.Module
  , types     :: Map Ref.Type Type
  , constants :: Map Ref.Value (Interpreter.Closure, Type)
  , order     :: Seq Ref.Value
  , imports   :: Map Ref.Module Module
  } deriving (Generic, Show)

instance Pretty Module where
  pretty Module {name, types, constants, order} =
    let longest = maximum $ map (length . show . pretty) order
        intro = "module" <+> pretty name
        types' = lined $ map (\(typename, expr) -> hsep [pretty typename, "=", pretty expr]) $ toPairs types
        constants' = lined $ mapMaybe (\binding -> do
                                          (_, typ) <- Map.lookup binding constants
                                          pure $ hsep [fill longest $ pretty binding, ":", pretty typ]) $ toList order
        content = braces $ align $ mconcat [types', line', constants'] -- FIXME: empty line when there are no typedefs
    in parens $ vsep [intro, indent 2 content]
    where lined = mconcat . punctuate hardline

native, empty :: Module

native = Module
  { name      = Ref.Module ["lang", "core", "native"]
  , types     = Type.natives
  , constants = map
    (\bi ->
      let closure = Interpreter.Closure (Term.Extra () $ Interpreter.Native bi []) mempty
          typ     = Builtins.typ bi
      in  (closure, typ)
    )
    Builtins.builtins
  , order     = mempty
  , imports   = mempty
  }

empty = Module
  { name      = Ref.Module []
  , types     = mempty
  , constants = mempty
  , order     = mempty
  , imports   = Map.singleton (name native) native
  }

typedefs :: Module -> Map Ref.Type Type
typedefs modul =
  let locals   = types modul
      imported = foldMap typedefs $ elems $ imports modul
  in  locals <> imported

bindings :: Module -> Map Ref.Value Type
bindings modul =
  let locals   = map snd $ constants modul
      imported = foldMap bindings $ elems $ imports modul
  in  locals <> imported

values :: Module -> Map Ref.Value Interpreter.Closure
values modul =
  let locals   = map fst $ constants modul
      imported = foldMap values $ elems $ imports modul
  in  locals <> imported

load :: (Type.MonadInfer s m) => Module -> Definition phase -> m Module
load modul@Module { constants, order } (Def.Constant _ name body) = do
  let ctx   = Ctx.initialize $ bindings modul
      body' = meta (const ()) body
  ((typ, _), _) <- Type.synthesize ctx body'
  let closure    = Interpreter.Closure (meta (const ()) body') $ values modul
      constants' = Map.insert name (closure, typ) constants
      order'     = order |> name
  pure $ modul { constants = constants', order = order' }
load modul@Module { types } typedef@(Def.Type _ name _ _) = do
  (typ, _) <- Type.fromDefinition (typedefs modul) typedef
  let types' = Map.insert name typ types
  pure $ modul { types = types' }
load modul (Def.Module _ name definitions) = foldM load modul { name } definitions

run :: Def.Definition phase -> Either Error Term
run modul@Def.Module{} = do
  Interpreter.Closure main env <-
    liftMaybe (Module NoMainFunction) $ Map.lookup (Syntax.Local "main") $ load functions modul
  meta (const ()) <$> Interpreter.evalWith (Map.union env functions) main
