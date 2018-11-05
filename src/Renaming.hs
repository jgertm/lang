module Renaming
  ( rename
  , renameWith
  , Renaming
  )
where

import qualified Data.Map.Strict               as Map

import           Builtins
import           Error

import           Syntax

data Renaming

type instance Context Renaming = Maybe Binding

type Term = Term' Renaming

type Definition = Definition' Renaming

type MonadRename m = (MonadError RenamingError m, MonadState Env m)

type Scope = Set Binding

type Env = Map Binding Int

rename :: (Renameable t) => t phase -> Either Error (t Renaming)
rename = renameWith $ map (const 0) builtins

renameWith :: (Renameable t) => Env -> t phase -> Either Error (t Renaming)
renameWith env = runRenaming env . renameNode

runRenaming :: Env -> _ a -> Either Error a
runRenaming env = runExcept . withExcept Renaming . evaluatingStateT env

alias :: (MonadRename m) => Binding -> m Binding
alias binding@(Single name) = do
  env <- get
  pure . Single $ case Map.lookup binding env of
    Nothing -> name
    Just 0  -> name
    Just i  -> name <> show i

insertName :: Binding -> Map Binding Int -> Map Binding Int
insertName name = Map.insertWith (\old new -> old + new + 1) name 0

add :: (MonadRename m) => Binding -> m a -> m a
add binding action = do
  modify $ insertName binding
  action

adds :: (MonadRename m) => [Binding] -> m a -> m a
adds names action = do
  traverse_ (modify . insertName) names
  action

check :: (MonadRename m) => Binding -> m ()
check binding@(Single name) = do
  env <- get
  case Map.lookup binding env of
    Just _  -> pass
    Nothing -> throwError (UnknownSymbol name)

class Renameable t where
  renameNode :: (MonadRename m) => t phase -> m (t Renaming)

instance Renameable Term' where
  renameNode = renameTerm

renameTerm :: (MonadRename m) => Term' phase -> m Term
renameTerm (ESymbol _ name) = do
  check name
  name' <- alias name
  pure $ ESymbol (Just name) name'
renameTerm (ELambda _ args body) = adds args $ do
  args' <- traverse alias args
  body' <- renameTerm body
  pure $ ELambda Nothing args' body'
renameTerm (ELet _ bindings body) = do
  let go ((name, value) : moreBindings) = do
        value' :: Term <- renameTerm value
        add name $ do
          name'                   <- alias name
          (moreBindings', result) <- go moreBindings
          pure ((name', value') : moreBindings', result)
      go [] = sequenceA ([], renameTerm body)
  (bindings', body') <- go bindings
  pure $ ELet Nothing bindings' body'
renameTerm (EApplication _ fn args) = do
  fn'   <- renameTerm fn
  args' <- traverse renameTerm args
  pure $ EApplication Nothing fn' args'
renameTerm term = metaM (const $ pure Nothing) term

instance Renameable Definition' where
  renameNode = renameDefinition

renameDefinition :: (MonadRename m) => Definition' phase -> m Definition
renameDefinition (DModule _ name definitions) = do
  name'        <- alias name
  definitions' <- traverse renameDefinition definitions
  pure $ DModule (Just name) name' definitions'
renameDefinition (DFunction _ name arguments body) = do
  name'      <- alias name
  arguments' <- traverse alias arguments
  body'      <- adds arguments $ renameNode body
  pure $ DFunction (Just name) name' arguments' body'
renameDefinition (DConstant _ name body) = do
  name' <- alias name
  body' <- renameNode body
  pure $ DConstant (Just name) name' body'
