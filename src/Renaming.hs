module Renaming
  ( rename
  , renameWith
  , Renaming
  )
where

import qualified Data.Map.Strict               as Map

import           Builtins
import           Classes
import           Error
import qualified Syntax.Definition             as Definition
import qualified Syntax.Pattern                as Pattern
import qualified Syntax.Reference              as Reference
import qualified Syntax.Term                   as Term
import qualified Syntax.Type                   as Type


data Renaming

type instance Context Renaming = Maybe Value

type Term = Term.Term Renaming
type Pattern = Pattern.Pattern Renaming
type Definition = Definition.Definition Renaming
type Value = Reference.Value

type MonadRename m = (MonadError RenamingError m, MonadState Env m)

type Env = Map Value Int

rename :: (Renameable t) => t phase -> Either Error (t Renaming)
rename = renameWith $ map (const 0) builtins

renameWith :: (Renameable t) => Env -> t phase -> Either Error (t Renaming)
renameWith env = runRenaming env . renameNode

runRenaming :: Env -> StateT Env (Except RenamingError) a -> Either Error a
runRenaming env = runExcept . withExcept Renaming . evaluatingStateT env

alias :: (MonadRename m) => Value -> m Value
alias name = do
  env <- get
  pure $ case Map.lookup name env of
    Nothing -> name
    Just 0  -> name
    Just i  -> name `Reference.append` show i

insertName :: Value -> Map Value Int -> Map Value Int
insertName name = Map.insertWith (\old new -> old + new + 1) name 0

add :: (MonadRename m) => Value -> m a -> m a
add name action = do
  modify $ insertName name
  action

adds :: (MonadRename m) => [Value] -> m a -> m a
adds names action = do
  traverse_ (modify . insertName) names
  action

check :: (MonadRename m) => Value -> m ()
check name = do
  env <- get
  case Map.lookup name env of
    Just _  -> pass
    Nothing -> throwError (UnknownSymbol name)

class Renameable t where
  renameNode :: (MonadRename m) => t phase -> m (t Renaming)

instance Renameable Term.Term where
  renameNode = renameTerm

renameTerm :: (MonadRename m) => Term.Term phase -> m Term
renameTerm (Term.Symbol _ name) = do
  check name
  name' <- alias name
  pure $ Term.Symbol (Just name) name'
renameTerm (Term.Lambda _ arg body) = add arg $ do
  arg'  <- alias arg
  body' <- renameTerm body
  pure $ Term.Lambda Nothing arg' body'
renameTerm (Term.Let _ bindings body) = do
  let go ((name, value) : moreBindings) = do
        value' :: Term <- renameTerm value
        add name $ do
          name'                   <- alias name
          (moreBindings', result) <- go moreBindings
          pure ((name', value') : moreBindings', result)
      go [] = sequenceA ([], renameTerm body)
  (bindings', body') <- go bindings
  pure $ Term.Let Nothing bindings' body'
renameTerm (Term.Application _ fn args) = do
  fn'   <- renameTerm fn
  args' <- traverse renameTerm args
  pure $ Term.Application Nothing fn' args'
renameTerm (Term.Match _ prototype branches) = do
  prototype' <- renameTerm prototype
  branches'  <- forM branches $ \Term.Branch { patterns, body } -> do
    patterns' <- traverse renamePattern patterns
    body'     <- renameTerm body
    pure $ Term.Branch {patterns = patterns', body = body'}
  pure $ Term.Match Nothing prototype' branches'
renameTerm term = walkM renameTerm $ meta (const Nothing) term


instance Renameable Definition.Definition where
  renameNode = renameDefinition

renameDefinition :: (MonadRename m) => Definition.Definition phase -> m Definition
renameDefinition (Definition.Module _ name definitions) = do
  definitions' <- traverse renameDefinition definitions
  pure $ Definition.Module Nothing name definitions'
renameDefinition (Definition.Type _ name typ) =
  pure $ Definition.Type Nothing name (meta (const Nothing) typ)
renameDefinition (Definition.Constant _ name body) = do
  name' <- alias name
  body' <- renameNode body
  pure $ Definition.Constant (Just name) name' body'


instance Renameable Pattern.Pattern where
  renameNode = renamePattern

renamePattern :: (MonadRename m) => Pattern.Pattern phase -> m Pattern
renamePattern (Pattern.Symbol _ name) = add name $ do
  name' <- alias name
  pure $ Pattern.Symbol (Just name) name'
renamePattern pattrn = walkM renamePattern $ meta (const Nothing) pattrn
