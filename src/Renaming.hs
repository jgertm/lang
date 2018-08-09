module Renaming
  ( rename
  , renameWith
  , Renaming
  ) where

import qualified Data.Map.Strict as M

import           Builtins
import           Error
import           Syntax

data Renaming

type instance Context Renaming = Maybe Binding

type Term = Term' Renaming

type MonadRename m = (MonadError RenamingError m, MonadReader Env m)

type Env = Map Binding Int

newtype Rename a = Rename
  { unRename :: ReaderT Env (Except RenamingError) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError RenamingError
             , MonadReader Env
             )

rename :: Term' phase -> Either Error Term
rename = renameWith $ map (const 0) builtins

renameWith :: Env -> Term' phase -> Either Error Term
renameWith env = runRenaming env . renameTerm

runRenaming :: Env -> Rename a -> Either Error a
runRenaming env = runExcept . withExcept Renaming . usingReaderT env . unRename

alias :: (MonadRename m) => Binding -> m Binding
alias binding@(Single name) = do
  env <- ask
  pure . Single $
    case M.lookup binding env of
      Nothing -> name
      Just 0  -> name
      Just i  -> name <> show i

insertName :: Binding -> Map Binding Int -> Map Binding Int
insertName name = M.insertWith (\old new -> old + new + 1) name 0

add :: (MonadRename m) => Binding -> m a -> m a
add binding = local $ insertName binding

adds :: (MonadRename m) => [Binding] -> m a -> m a
adds names = local $ \env -> foldl (flip insertName) env names

check :: (MonadRename m) => Binding -> m ()
check binding@(Single name) = do
  env <- ask
  case M.lookup binding env of
    Just _  -> pass
    Nothing -> throwError (UnknownSymbol name)

renameTerm :: (MonadRename m) => Term' phase -> m Term
renameTerm (ESymbol _ name) = do
  check name
  name' <- alias name
  pure $ ESymbol (Just name) name'
renameTerm (ELambda _ args body) =
  adds args $ do
    args' <- traverse alias args
    body' <- renameTerm body
    pure $ ELambda Nothing args' body'
renameTerm (ELet _ bindings body) = do
  let go ((name, value):moreBindings) = do
        value' :: Term <- renameTerm value
        add name $ do
          name' <- alias name
          (moreBindings', result) <- go moreBindings
          pure ((name', value') : moreBindings', result)
      go [] = sequenceA ([], renameTerm body)
  (bindings', body') <- go bindings
  pure $ ELet Nothing bindings' body'
renameTerm (EApplication _ fn args) = do
  fn' <- renameTerm fn
  args' <- traverse renameTerm args
  pure $ EApplication Nothing fn' args'
renameTerm term = metaM (const $ pure Nothing) term
